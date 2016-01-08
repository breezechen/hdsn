//======================================================================
//
// Copyright (C) 1996-1998 Mark Russinovich and Bryce Cogswell  !!!!!!!
//                              -----------     --------------
//
// MODIFIED : WANG Nianhua ,2006/10/10
//
//======================================================================
#include "ntddk.h"

//----------------------------------------------------------------------
//                         GLOBALS
//----------------------------------------------------------------------
// our user-inteface device object
PDEVICE_OBJECT          GUIDevice;
BOOLEAN                 GUIActive = FALSE;

BOOLEAN                 IsHooked  = FALSE;

#define MAX_HD_COUNT    16
#define SN_LEN          20

typedef struct _SerialNumbers
{
    UCHAR DiskSerial[SN_LEN];
    UCHAR ChangeTo  [SN_LEN];
}SerialNumbers, *PSerialNumbers;


SerialNumbers SNS[MAX_HD_COUNT] = {0};


typedef struct _IDINFO
{
    USHORT  wGenConfig;     // WORD 0: 基本信息字
    USHORT  wNumCyls;     // WORD 1: 柱面数
    USHORT  wReserved2;     // WORD 2: 保留
    USHORT  wNumHeads;     // WORD 3: 磁头数
    USHORT  wReserved4;        // WORD 4: 保留
    USHORT  wReserved5;        // WORD 5: 保留
    USHORT  wNumSectorsPerTrack;  // WORD 6: 每磁道扇区数
    USHORT  wVendorUnique[3];   // WORD 7-9: 厂家设定值
    CHAR    sSerialNumber[20];   // WORD 10-19:序列号
    USHORT  wBufferType;    // WORD 20: 缓冲类型
    USHORT  wBufferSize;    // WORD 21: 缓冲大小
    USHORT  wECCSize;     // WORD 22: ECC校验大小
    CHAR    sFirmwareRev[8];   // WORD 23-26: 固件版本
    CHAR    sModelNumber[40];   // WORD 27-46: 内部型号
    USHORT  wMoreVendorUnique;   // WORD 47: 厂家设定值
    USHORT  wReserved48;    // WORD 48: 保留
    struct {
        USHORT  reserved1:8;
        USHORT  DMA:1;     // 1=支持DMA
        USHORT  LBA:1;     // 1=支持LBA
        USHORT  DisIORDY:1;    // 1=可不使用IORDY
        USHORT  IORDY:1;    // 1=支持IORDY
        USHORT  SoftReset:1;   // 1=需要ATA软启动
        USHORT  Overlap:1;    // 1=支持重叠操作
        USHORT  Queue:1;    // 1=支持命令队列
        USHORT  InlDMA:1;    // 1=支持交叉存取DMA
    } wCapabilities;     // WORD 49: 一般能力
    USHORT  wReserved1;     // WORD 50: 保留
    USHORT  wPIOTiming;     // WORD 51: PIO时序
    USHORT  wDMATiming;     // WORD 52: DMA时序
    struct {
        USHORT  CHSNumber:1;   // 1=WORD 54-58有效
        USHORT  CycleNumber:1;   // 1=WORD 64-70有效
        USHORT  UnltraDMA:1;   // 1=WORD 88有效
        USHORT  reserved:13;
    } wFieldValidity;     // WORD 53: 后续字段有效性标志
    USHORT  wNumCurCyls;    // WORD 54: CHS可寻址的柱面数
    USHORT  wNumCurHeads;    // WORD 55: CHS可寻址的磁头数
    USHORT  wNumCurSectorsPerTrack;  // WORD 56: CHS可寻址每磁道扇区数
    USHORT  wCurSectorsLow;    // WORD 57: CHS可寻址的扇区数低位字
    USHORT  wCurSectorsHigh;   // WORD 58: CHS可寻址的扇区数高位字
    struct {
        USHORT  CurNumber:8;   // 当前一次性可读写扇区数
        USHORT  Multi:1;    // 1=已选择多扇区读写
        USHORT  reserved1:7;
    } wMultSectorStuff;     // WORD 59: 多扇区读写设定
    ULONG  dwTotalSectors;    // WORD 60-61: LBA可寻址的扇区数
    USHORT  wSingleWordDMA;    // WORD 62: 单字节DMA支持能力
    struct {
        USHORT  Mode0:1;    // 1=支持模式0 (4.17Mb/s)
        USHORT  Mode1:1;    // 1=支持模式1 (13.3Mb/s)
        USHORT  Mode2:1;    // 1=支持模式2 (16.7Mb/s)
        USHORT  Reserved1:5;
        USHORT  Mode0Sel:1;    // 1=已选择模式0
        USHORT  Mode1Sel:1;    // 1=已选择模式1
        USHORT  Mode2Sel:1;    // 1=已选择模式2
        USHORT  Reserved2:5;
    } wMultiWordDMA;     // WORD 63: 多字节DMA支持能力
    struct {
        USHORT  AdvPOIModes:8;   // 支持高级POI模式数
        USHORT  reserved:8;
    } wPIOCapacity;      // WORD 64: 高级PIO支持能力
    USHORT  wMinMultiWordDMACycle;  // WORD 65: 多字节DMA传输周期的最小值
    USHORT  wRecMultiWordDMACycle;  // WORD 66: 多字节DMA传输周期的建议值
    USHORT  wMinPIONoFlowCycle;   // WORD 67: 无流控制时PIO传输周期的最小值
    USHORT  wMinPOIFlowCycle;   // WORD 68: 有流控制时PIO传输周期的最小值
    USHORT  wReserved69[11];   // WORD 69-79: 保留
    struct {
        USHORT  Reserved1:1;
        USHORT  ATA1:1;     // 1=支持ATA-1
        USHORT  ATA2:1;     // 1=支持ATA-2
        USHORT  ATA3:1;     // 1=支持ATA-3
        USHORT  ATA4:1;     // 1=支持ATA/ATAPI-4
        USHORT  ATA5:1;     // 1=支持ATA/ATAPI-5
        USHORT  ATA6:1;     // 1=支持ATA/ATAPI-6
        USHORT  ATA7:1;     // 1=支持ATA/ATAPI-7
        USHORT  ATA8:1;     // 1=支持ATA/ATAPI-8
        USHORT  ATA9:1;     // 1=支持ATA/ATAPI-9
        USHORT  ATA10:1;    // 1=支持ATA/ATAPI-10
        USHORT  ATA11:1;    // 1=支持ATA/ATAPI-11
        USHORT  ATA12:1;    // 1=支持ATA/ATAPI-12
        USHORT  ATA13:1;    // 1=支持ATA/ATAPI-13
        USHORT  ATA14:1;    // 1=支持ATA/ATAPI-14
        USHORT  Reserved2:1;
    } wMajorVersion;     // WORD 80: 主版本
    USHORT  wMinorVersion;    // WORD 81: 副版本
    USHORT  wReserved82[6];    // WORD 82-87: 保留
    struct {
        USHORT  Mode0:1;    // 1=支持模式0 (16.7Mb/s)
        USHORT  Mode1:1;    // 1=支持模式1 (25Mb/s)
        USHORT  Mode2:1;    // 1=支持模式2 (33Mb/s)
        USHORT  Mode3:1;    // 1=支持模式3 (44Mb/s)
        USHORT  Mode4:1;    // 1=支持模式4 (66Mb/s)
        USHORT  Mode5:1;    // 1=支持模式5 (100Mb/s)
        USHORT  Mode6:1;    // 1=支持模式6 (133Mb/s)
        USHORT  Mode7:1;    // 1=支持模式7 (166Mb/s) ???
        USHORT  Mode0Sel:1;    // 1=已选择模式0
        USHORT  Mode1Sel:1;    // 1=已选择模式1
        USHORT  Mode2Sel:1;    // 1=已选择模式2
        USHORT  Mode3Sel:1;    // 1=已选择模式3
        USHORT  Mode4Sel:1;    // 1=已选择模式4
        USHORT  Mode5Sel:1;    // 1=已选择模式5
        USHORT  Mode6Sel:1;    // 1=已选择模式6
        USHORT  Mode7Sel:1;    // 1=已选择模式7
    } wUltraDMA;      // WORD 88:  Ultra DMA支持能力
    USHORT    wReserved89[167];   // WORD 89-255
} IDINFO, *PIDINFO;

#pragma pack(push, 1)

typedef struct _DRIVERSTATUS {
        BYTE     bDriverError;           // Error code from driver,
                                                                // or 0 if no error.
        BYTE     bIDEError;                      // Contents of IDE Error register.
                                                                // Only valid when bDriverError
                                                                // is SMART_IDE_ERROR.
        BYTE     bReserved[2];           // Reserved for future expansion.
        DWORD   dwReserved[2];          // Reserved for future expansion.
} DRIVERSTATUS, *PDRIVERSTATUS, *LPDRIVERSTATUS;

typedef struct _SENDCMDOUTPARAMS {
        DWORD                   cBufferSize;            // Size of bBuffer in bytes
        DRIVERSTATUS            DriverStatus;           // Driver status structure.
        BYTE                    bBuffer[1];             // Buffer of arbitrary length in which to store the data read from the                                                                                  // drive.
} SENDCMDOUTPARAMS, *PSENDCMDOUTPARAMS, *LPSENDCMDOUTPARAMS;

#pragma pack(pop)


PDRIVER_DISPATCH RealDiskDeviceControl = NULL;
PDRIVER_OBJECT DiskDriver = NULL;

NTSTATUS HookedDiskDeviceControl(PDEVICE_OBJECT DeviceObject, PIRP Irp)
{
    PIO_STACK_LOCATION     irpStack = IoGetCurrentIrpStackLocation(Irp);
    NTSTATUS               status = RealDiskDeviceControl(DeviceObject, Irp);
    if (NT_SUCCESS(status) && irpStack->Parameters.DeviceIoControl.IoControlCode == SMART_RCV_DRIVE_DATA) {
        IDINFO* info = (IDINFO*)((SENDCMDOUTPARAMS*)Irp->AssociatedIrp.SystemBuffer)->bBuffer;
        for (int i = 0; i < MAX_HD_COUNT; ++i)
        {
            if (memcmp(info->sSerialNumber, SNS[i].DiskSerial, 20) == 0) {
                RtlMoveMemory(info->sSerialNumber, SNS[i].ChangeTo, 20);
                break;
            }
        }
    }
    return status;
}

BOOLEAN HookDiskDriver()
{
    UNICODE_STRING driverName;
    BOOLEAN ret = FALSE;
    NTSTATUS status;

    RtlInitUnicodeString(&driverName, L"\\driver\\disk");

    status = ObReferenceObjectByName( 
        &driverName, 
        OBJ_CASE_INSENSITIVE, 
        NULL, 
        0, 
        *IoDriverObjectType, 
        KernelMode,
        NULL, 
        (PVOID *)&DiskDriver); 

    do 
    {
        if (DiskDriver == NULL) {
            MyDbgPrint("[br] in [%s] ObReferenceObjectByName失败\n", __FUNCTION__);
            break;
        }

        RealDiskDeviceControl = DiskDriver->MajorFunction[IRP_MJ_DEVICE_CONTROL];
        InterlockedExchangePointer((volatile PVOID *)&DiskDriver->MajorFunction[IRP_MJ_DEVICE_CONTROL], HookedDiskDeviceControl);
        ret = TRUE;
    } while (0);

    return ret;
}

VOID UnhookDiskDriver()
{
    InterlockedExchangePointer((volatile PVOID *)&DiskDriver->MajorFunction[IRP_MJ_READ], RealDiskDeviceControl);
    ObDereferenceObject(DiskDriver);
    DiskDriver = NULL;
}

//----------------------------------------------------------------------
//
// Replaces entries in the system service table with pointers to
// our own hook routines. We save off the real routine addresses.
//
//----------------------------------------------------------------------
BOOLEAN HookStart( void )
{
    if( !IsHooked ) {
        IsHooked = HookDiskDriver();
    }
    return IsHooked;
}


VOID HookStop( )
{
    if( IsHooked ) {
        UnhookDiskDriver();
        IsHooked = FALSE;
    }
}


//======================================================================
//         D E V I C E - D R I V E R  R O U T I N E S
//======================================================================
BOOLEAN  HDHookDeviceControl( IN PFILE_OBJECT FileObject, IN BOOLEAN Wait,
                              IN PVOID InputBuffer, IN ULONG InputBufferLength,
                              OUT PVOID OutputBuffer, IN ULONG OutputBufferLength,
                              IN ULONG IoControlCode, OUT PIO_STATUS_BLOCK IoStatus,
                              IN PDEVICE_OBJECT DeviceObject ) {
    BOOLEAN                 retval = FALSE;
    ULONG i;

    // Its a message from our GUI!
    IoStatus->Status      = STATUS_SUCCESS; // Assume success
    IoStatus->Information = 0;              // Assume nothing returned
    PSNInfo psn = NULL;


    switch ( IoControlCode ) {
    case HDHOOK_HOOK:
        HookStart();
        break;

    case HDHOOK_UNHOOK:
        HookStop();
        break;

    case HDHOOK_SETSELFVALUE:
      if( InputBufferLength < SN_LEN || InputBuffer == NULL){
        IoStatus->Status = STATUS_INVALID_PARAMETER;
        break;
      }
      psn = (PSNInfo)inputBuffer;
      if (psn->index >= MAX_HD_COUNT) {
        IoStatus->Status = STATUS_INVALID_PARAMETER;
        break;
      }
      for(i=0;i< SN_LEN ;i++)
        SNS[psn->index].DiskSerial[i] = psn->sn[i];
        break;
    case HDHOOK_SETEMULABLEVALUE:
      if( InputBufferLength < SN_LEN || InputBuffer == NULL){
        IoStatus->Status = STATUS_INVALID_PARAMETER;
        break;
      }
      psn = (PSNInfo)inputBuffer;
      if (psn->index >= MAX_HD_COUNT) {
        IoStatus->Status = STATUS_INVALID_PARAMETER;
        break;
      }
      for(i=0;i< SN_LEN ;i++)
        SNS[psn->index].ChangeTo[i] = psn->sn[i];
        break;
  case HDHOOK_VERSION:
        if ( OutputBufferLength < sizeof(ULONG) ||
             OutputBuffer == NULL ) {
            IoStatus->Status = STATUS_INVALID_PARAMETER;
            break;
        }

        *(ULONG *)OutputBuffer = REGMONVERSION;
        IoStatus->Information = sizeof(ULONG);
        break;

    default:
        IoStatus->Status = STATUS_INVALID_DEVICE_REQUEST;
        break;
    }
    return TRUE;
}


//----------------------------------------------------------------------
//
// In this routine we handle requests to our own device. The only
// requests we care about handling explicitely are IOCTL commands that
// we will get from the GUI. We also expect to get Create and Close
// commands when the GUI opens and closes communications with us.
//----------------------------------------------------------------------
NTSTATUS HDHookDispatch( IN PDEVICE_OBJECT DeviceObject, IN PIRP Irp )
{
    PIO_STACK_LOCATION      irpStack;
    PVOID                   inputBuffer;
    PVOID                   outputBuffer;
    ULONG                   inputBufferLength;
    ULONG                   outputBufferLength;
    ULONG                   ioControlCode;
    WORK_QUEUE_ITEM         workItem;

    // Go ahead and set the request up as successful
    Irp->IoStatus.Status      = STATUS_SUCCESS;
    Irp->IoStatus.Information = 0;

    // Get a pointer to the current location in the Irp. This is where
    //     the function codes and parameters are located.
    irpStack = IoGetCurrentIrpStackLocation (Irp);

    // Get the pointer to the input/output buffer and its length
    inputBuffer             = Irp->AssociatedIrp.SystemBuffer;
    inputBufferLength       = irpStack->Parameters.DeviceIoControl.InputBufferLength;
    outputBuffer            = Irp->AssociatedIrp.SystemBuffer;
    outputBufferLength      = irpStack->Parameters.DeviceIoControl.OutputBufferLength;
    ioControlCode           = irpStack->Parameters.DeviceIoControl.IoControlCode;

    switch (irpStack->MajorFunction) {
    case IRP_MJ_CREATE:
        GUIActive = TRUE;
        break;

    case IRP_MJ_SHUTDOWN:
        break;

    case IRP_MJ_CLOSE:
        GUIActive = FALSE;
        break;

    case IRP_MJ_DEVICE_CONTROL:
        // See if the output buffer is really a user buffer that we
        // can just dump data into.
        if( IOCTL_TRANSFER_TYPE(ioControlCode) == METHOD_NEITHER ) {

            outputBuffer = Irp->UserBuffer;
        }

        // Its a request from the GUI
        HDHookDeviceControl( irpStack->FileObject, TRUE,
                             inputBuffer, inputBufferLength,
                             outputBuffer, outputBufferLength,
                             ioControlCode, &Irp->IoStatus, DeviceObject );
        break;
    }
    IoCompleteRequest( Irp, IO_NO_INCREMENT );
    return STATUS_SUCCESS;
}


//----------------------------------------------------------------------
//
// RegmonUnload
//
// Our job is done - time to leave.
//
//----------------------------------------------------------------------
VOID RegmonUnload( IN PDRIVER_OBJECT DriverObject )
{
    WCHAR                   deviceLinkBuffer[]  = L"\\DosDevices\\"DRIVER_NAME;
    UNICODE_STRING          deviceLinkUnicodeString;

    // Unhook the registry
    if( IsHooked ) HookStop();

    // Delete the symbolic link for our device
    RtlInitUnicodeString( &deviceLinkUnicodeString, deviceLinkBuffer );
    IoDeleteSymbolicLink( &deviceLinkUnicodeString );

    // Delete the device object
    IoDeleteDevice( DriverObject->DeviceObject );

}

//----------------------------------------------------------------------
//
// DriverEntry
//
// Installable driver initialization. Here we just set ourselves up.
//
//----------------------------------------------------------------------
NTSTATUS DriverEntry(IN PDRIVER_OBJECT DriverObject, IN PUNICODE_STRING RegistryPath )
{
    NTSTATUS                ntStatus;
    WCHAR                   deviceNameBuffer[]  = L"\\Device\\"DRIVER_NAME;
    UNICODE_STRING          deviceNameUnicodeString;
    WCHAR                   deviceLinkBuffer[]  = L"\\DosDevices\\"DRIVER_NAME;
    UNICODE_STRING          deviceLinkUnicodeString;
    WCHAR                   startValueBuffer[] = L"Start";
    UNICODE_STRING          startValueUnicodeString;

    UNICODE_STRING          registryPath;
    HANDLE                  driverKey;
    ULONG                   startType, demandStart;
    RTL_QUERY_REGISTRY_TABLE paramTable[2];
    OBJECT_ATTRIBUTES       objectAttributes;
    int                     i;

    // Query our start type to see if we are supposed to monitor starting
    // at boot time
    registryPath.Buffer = ExAllocatePool( PagedPool,
                                          RegistryPath->Length + sizeof(UNICODE_NULL));

    if (!registryPath.Buffer) {
        return STATUS_INSUFFICIENT_RESOURCES;
    }

    registryPath.Length = RegistryPath->Length + sizeof(UNICODE_NULL);
    registryPath.MaximumLength = registryPath.Length;

    RtlZeroMemory( registryPath.Buffer, registryPath.Length );

    RtlMoveMemory( registryPath.Buffer,  RegistryPath->Buffer,
                   RegistryPath->Length  );

    RtlZeroMemory( &paramTable[0], sizeof(paramTable));
    paramTable[0].Flags = RTL_QUERY_REGISTRY_DIRECT;
    paramTable[0].Name = L"Start";
    paramTable[0].EntryContext = &startType;
    paramTable[0].DefaultType = REG_DWORD;
    paramTable[0].DefaultData = &startType;
    paramTable[0].DefaultLength = sizeof(ULONG);

    RtlQueryRegistryValues( RTL_REGISTRY_ABSOLUTE,
                            registryPath.Buffer, &paramTable[0],
                            NULL, NULL  );

    // Set start type to demand start so that boot logging
    // only happens this boot (unless the user reconfigures it in
    // the GUI)
    InitializeObjectAttributes( &objectAttributes, RegistryPath,
                                OBJ_CASE_INSENSITIVE, NULL, NULL );
    ntStatus = ZwOpenKey( &driverKey, KEY_WRITE, &objectAttributes );
    if( NT_SUCCESS( ntStatus )) {

        demandStart = SERVICE_DEMAND_START;
        RtlInitUnicodeString( &startValueUnicodeString, startValueBuffer );
        ZwSetValueKey( driverKey, &startValueUnicodeString, 0, REG_DWORD,
                       &demandStart, sizeof(demandStart ));
        ZwClose( driverKey );
    }

    // Setup our name and symbolic link
    RtlInitUnicodeString (&deviceNameUnicodeString,
                          deviceNameBuffer );
    RtlInitUnicodeString (&deviceLinkUnicodeString,
                          deviceLinkBuffer );

    // Set up the device used for GUI communications
    ntStatus = IoCreateDevice ( DriverObject,
                                0,
                                &deviceNameUnicodeString,
                                FILE_DEVICE_REGMON,
                                0,
                                TRUE,
                                &GUIDevice );
    if (NT_SUCCESS(ntStatus)) {

        // Create a symbolic link that the GUI can specify to gain access
        // to this driver/device
        ntStatus = IoCreateSymbolicLink (&deviceLinkUnicodeString,
                                         &deviceNameUnicodeString );

        // Create dispatch points for all routines that must be handled
        DriverObject->MajorFunction[IRP_MJ_SHUTDOWN]        =
        DriverObject->MajorFunction[IRP_MJ_CREATE]          =
        DriverObject->MajorFunction[IRP_MJ_CLOSE]           =
        DriverObject->MajorFunction[IRP_MJ_DEVICE_CONTROL]  = HDHookDispatch;
#if DBG
        DriverObject->DriverUnload                          = RegmonUnload;
#endif
    }
    if (!NT_SUCCESS(ntStatus)) {

        // Something went wrong, so clean up (free resources etc)
        if( GUIDevice ) IoDeleteDevice( GUIDevice );
        IoDeleteSymbolicLink( &deviceLinkUnicodeString );
        return ntStatus;
    }
    // Pointer to system table data structure is an NTOSKRNL export
    ServiceTable = KeServiceDescriptorTable;

    return STATUS_SUCCESS;
}

