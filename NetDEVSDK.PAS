unit NetDEVSDK;

interface

uses System.Classes, System.SysUtils, Winapi.Windows, System.IOUtils, System.Generics.Collections,
  JdcLogging, JdcGlobal, System.DateUtils, Vcl.ExtCtrls, Vcl.Forms, Vcl.Controls, System.Threading, JdcNetSDK;

const
  NETDEVSDK_DLL = 'NetDEVSDK.DLL';

  NETDEV_PICTURE_BMP = 0;
  NETDEV_PICTURE_JPG = 1;

  NETDEV_LEN_64 = 63;
  NETDEV_LEN_128 = 127;
  NETDEV_LEN_132 = 131;
  NETDEV_LEN_235 = 235;
  NETDEV_LEN_256 = 255;
  NETDEV_LEN_260 = 259;

  NETDEV_LOGIN_PROTO_ONVIF = 0;
  NETDEV_DTYPE_IPC = 1;
  NETDEV_DTYPE_NVR = 101;

  ERROR_CODE: array [0 .. 514, 0 .. 1] of string = //
    (
    // UDNS
    ('3387', 'No Data'), //
    // Exception callback message types Enumeration definition
    ('300', 'Playback ended'), //
    ('301', 'Playback exception occured'), //
    ('302', 'Backup ended'), //
    ('303', 'Disk removed'), //
    ('304', 'Disk full'), //
    ('305', 'Backup failure caused by other reasons'),

    // Common Error Codes
    ('-1', 'Failed'), //
    ('0', 'No Error'), //
    ('1', 'Common Failed'), //
    ('5', 'User does not exist'), //

    ('11', 'No result'), //
    ('12', 'Buffer is too small for receiving device data'), //
    ('13', 'socket listen  Failed to create socket listen'), //
    ('14', 'Failed to initialize lock'), //
    ('15', 'Failed to initialize semaphore'), //
    ('16', 'SDK  Error occurred during SDK resource allocation'), //
    ('17', 'Data not all sent'), //
    ('18', 'More data required'), //
    ('19', 'File transmission failed'), //
    ('20', 'Device type that are not supported'), //
    ('21', 'nonce Nonce expired'), //
    ('22', 'System internal error'), //
    ('24', 'Failed to bind alarms'), //

    ('100', 'errno  Failed to call system function. See errno'), //
    ('101', 'Null pointer'), //
    ('102', 'Invalid parameter'), //
    ('103', 'ID  Invalid module ID'), //
    ('104', 'Invalid handle'), //
    ('105', 'Memory allocation failed'), //
    ('106', 'File does not exist'), //
    ('107', 'Device does not exist'), //
    ('108', 'Qualified logs do not exist'), //
    ('109', 'busy busy status'), //
    ('110', 'Register timer failed'), //
    ('111', 'General error'), //
    ('112', 'Command does not support'), //
    ('113', 'The device does not support this function'), //
    ('114', 'Overtime'), //
    ('115', 'Message mismatch'), //
    ('116', 'Module does not exist'), //
    ('117', 'Message acceptance failure'), //
    ('118', 'IE Failure to get message IE'), //
    ('119', 'IE Failed to add message IE'), //
    ('120', 'SDK SDK uninitialized'), //
    ('121', 'SDK SDK has been initialized'), //
    ('122', 'Unsupported equipment manufacturer'), //
    ('123', 'Name already exists'), //
    ('124', 'Error acquiring configuration information'), //
    ('125', 'Error setting configuration information'), //
    ('126', 'Channel number exceeding specification'), //
    ('127', 'Call driver universal failure'), //
    ('128', 'Insufficient allocatable quota space'), //
    ('129', 'Failure to invoke database universality'), //
    ('130', 'Insufficient memory allocation'), //
    ('131', 'T2U Failure of T2U connection'), //
    ('132', 'Functions are being initialized'), //

    ('200', 'Failed to create connection'), //
    ('201', 'Failed to send message'), //
    ('202', 'Failed to decode response message'), //
    ('203', 'Function not supported'), //
    ('204', 'Json   Json common error'), //
    ('205', 'The query result is empty'), //
    ('206', 'Socket  Socket failed to receive message'), //
    ('207', 'Failed to create thread'), //
    ('208', 'Resource code not exist'), //
    ('209', 'Incorrect message content'), //
    ('210', 'Picture data is empty'), //
    ('211', 'Image size beyond the limit'), //
    ('403', 'The current platform is not support'), //

    ('60068', 'Failed to match publickey'), //

    // Media Stream Related Error Codes
    ('1001', 'Execution failure'), //
    ('1002', 'Illegal input parameters'), //
    ('1003', 'Insufficient system memory'), //
    ('1004', 'SOCKET Failure to create SOCKET'), //
    ('1005', 'Failure to receive'), //
    ('1006', 'Receive zero'), //
    ('1007', 'Function not supported for the time being'), //
    ('1008', 'Failed to create thread'), //
    ('1009', 'Failure to load dynamic library'), //
    ('1010', 'Failure to obtain dynamic methods'), //
    ('1011', 'Fail in send'), //
    ('1012', 'Insufficient permission to create files'), //
    ('1013', 'Reading file not found'), //
    ('1014', 'Log closes'), //
    ('1017', 'Media exception'), //

    // Player
    ('1257', 'Initialization player failed'), //
    ('1258', 'Failed to allocate playback channel resources'), //
    ('1259', 'Failed to obtain playback channel resources'), //
    ('1260', 'Cache queue full'), //
    ('1261', 'Cache queue empty'), //
    ('1262', 'Fail to open file'), //
    ('1263', 'The file has been read out'), //
    ('1264', 'Disk space is full'), //
    ('1265', 'Read failure'), //
    ('1266', 'Microphone does not exist'), //
    ('1267', 'TS TS packaging not completed'), //
    ('1268', 'The video has been saved.'), //
    ('1269', 'Resolution changed'), //
    ('1270', ' Recording is segmented '), //

    ('1513', 'Failed to start media stream playback'), //
    ('1514', 'Failed to shut down media stream playback'), //
    ('1515', 'Failure of receiving data due to network failure'), //
    ('1516', 'Media data processing failure'), //
    ('1517', 'Play channel did not start playback operation'), //
    ('1518', 'Input media streaming data failed'), //
    ('1519', 'Input data cached full'), //
    ('1520', 'Failed to set media stream data callback function'), //
    ('1521', 'Errors in Voice Service Operation'), //
    ('1522', 'Failure to start voice service'), //
    ('1523', 'Failed to shut down voice service'), //
    ('1524', 'Unknown media stream'), //
    ('1525', 'Packet loss'), //
    ('1526', 'Packing is not completed, more packages are needed'), //
    ('1527', 'Failed to create decoder'), //
    ('1528', 'Decoding failure'), //
    ('1529', 'Insufficient data received'), //
    ('1530', 'Display full resources'), //
    ('1531', 'Show that resources do not exist'), //
    ('1532', 'Resource creation failed'), //
    ('1533', 'Audio resources do not exist'), //
    ('1534', 'Decoder needs more data'), //
    ('1535', 'Failure to create encoder'), //
    ('1536', 'Collection resources do not exist'), //
    ('1537', 'The video has been turned on'), //
    ('1538', 'Undecoded, need to wait'), //
    ('1539', 'There is too much data to pack.'), //
    ('1540', 'AAC_LC AAC_LC decode failure'), //
    ('1541', 'Render surface lost '), //
    ('1542', 'Playback end'), //
    ('1543', 'File is encrypted '), //
    ('1544', 'Scrambling info failed '), //

    ('2000', 'Live business has been established'), //
    ('2001', 'Media streaming is not ready'), //
    ('2002', 'Live business display resources busy'), //
    ('2003', 'Real-time control block does not exist'), //
    ('2004', 'Real-time flow resources are full'), //
    ('2005', 'Session network error'), //
    ('2006', 'Session network timed out'), //
    ('2007', 'Session interaction error'), //
    ('2008', 'Authentication failed'), //
    ('2009', 'Internal processing error on device side'), //
    ('2010', 'Internal processing timed out'), //
    ('2011', 'Keep-alive failed'), //
    ('2012', 'Session does not exist'), //
    ('2013', 'Bandwidth not enough'), //
    ('2014', 'Live view already established'), //
    ('2015', 'Live view display resource is busy'), //
    ('2016', 'Multicast disabled'), //
    ('2017', 'Multicast port has been occupied'), //
    ('2018', 'Multicast port has been exhausted'), //
    ('2019', 'Multicast user does not exist'), //
    ('2020', 'Channel is offline'), //
    ('2021', 'Two-way audio resource code is invalid'), //
    ('2022', 'Audio resource is being used by two-way audio'), //
    ('2023', 'Two-way audio already exists'), //
    ('2024', 'Two-way audio does not exist'), //
    ('2025', 'Two-way audio timed out'), //
    ('2026', 'Two-way audio failed'), //
    ('2027', 'Undefined error'), //
    ('2028', 'Bad request'), //
    ('2029', 'Unauthorized'), //
    ('2030', 'Payment required'), //
    ('2031', 'Forbidden'), //
    ('2032', 'Method not allowed'), //
    ('2033', 'Not acceptable'), //
    ('2034', 'Proxy requires authentication'), //
    ('2035', 'Request timed out'), //
    ('2036', 'Not on server'), //
    ('2037', 'Not on server'), //
    ('2038', 'Pre-processing failed'), //
    ('2039', 'Entity too long'), //
    ('2040', 'URI too long'), //
    ('2041', 'UUnsupported media typeRI'), //
    ('2042', 'Parameter not understood'), //
    ('2043', 'Conference not found'), //
    ('2044', 'Bandwidth not enough'), //
    ('2045', 'Session not found'), //
    ('2046', 'Invalid method in this state'), //
    ('2047', 'Invalid header for the resource'), //
    ('2048', 'Invalid range'), //
    ('2049', 'Parameter is read only'), //
    ('2050', 'AO is not allowed'), //
    ('2051', 'Only AO is allowed'), //
    ('2052', 'Unsupported transport mode'), //
    ('2053', 'Destination is unreachable'), //
    ('2054', 'Server internal error'), //
    ('2055', 'Not implemented'), //
    ('2056', 'Gateway error'), //
    ('2057', 'Service unavailable'), //
    ('2058', 'Unsupported RTSP version'), //
    ('2059', 'Gateway timed out'), //
    ('2060', 'Unsupported option'), //
    ('2061', 'Failed to allocate memory'), //
    ('2062', 'Failed to re-allocate memory'), //
    ('2063', 'Describe timed out'), //
    ('2064', 'Channel is not bound'), //
    ('2065', 'Disk abnormal'), //

    ('2100', 'Speech intercom already exists'), //
    ('2101', 'Voice service does not exist'), //
    ('2102', 'Invalid encoding of voice intercom resources'), //
    ('2103', 'Audio resource is being used by two-way audio'), //
    ('2104', 'Speech intercom failure'), //
    ('2205', 'No more audio service allowed'), //

    ('2200', 'Snapshot format does not support'), //
    ('2201', 'Insufficient hard disk space'), //
    ('2202', 'Undecoded pictures can be captured'), //
    ('2203', 'Single snap operation failed'), //
    ('2301', 'Bit stream excess capability set'), //
    ('2793', 'Download finished '), //
    ('2794', 'failed to resolve domain name '), //
    ('2795', 'Connection failed '), //
    ('2796', 'Connection timed out '), //
    ('2797', 'Download timed out '), //
    ('2798', 'Download failed '), //
    ('2799', 'Poor network '), //
    ('2800', 'Playing finished '), //
    ('2801', 'Full disk space '), //
    ('2802', 'Authentication failed '), //
    ('2803', 'Current play time(for reporting use only)'), //
    ('2804', 'disk threshold Full '), //
    ('2805', 'time node not exist '), //
    ('2806', 'Buffer path not set '), //
    ('2807', 'failed to send message '), //
    ('2808', 'Task has been canceled '), //
    ('2809', 'Stream continues to play '), //

    ('10000', 'Media streaming is not ready'), //
    ('10001', 'Control block state unavailable'), //
    ('10002', 'Live business display resource busy'), //
    ('10003', 'Live media streams are not ready'), //
    ('10004', 'Playback media streams are not ready'), //

    ('10007', 'Insufficient network reception bandwidth'), //
    ('10008', 'Insufficient network transmission bandwidth'), //
    ('10009', 'Voice broadcasting service has reached the upper limit'), //
    ('10010', 'Audio channel has been occupied'), //

    ('10012', 'Encode type Not supported'), //

    ('10399', 'Maximum Media Related Error Code'), //

    // SnapShot
    ('27000', 'SNAPSHOT_PIC_FAILED'), //
    ('27001', 'SNAPSHOT_QRY_PIC_FAILED'), //
    ('27999', 'SNAPSHOT_MAX'), //

    // ERROR Codes of PTZ Module
    ('10400', 'Failure of preset setting'), //
    ('10401', 'Failure of preset query'), //
    ('10402', 'Trajectory Query Failure'), //
    ('10403', 'Failure to start recording trajectory'), //
    ('10404', 'Failure to end recording trajectory'), //
    ('10405', 'Cruise line query failed'), //
    ('10406', 'Failure to set up cruise routes'), //
    ('10407', 'PTZ operation failed'), //
    ('10408', 'Pre-positioning is used in cruise routes and cannot be deleted'), //
    ('10409', 'Setting Cruise Points Discontinuous'), //
    ('10410', 'Track is in use and cannot be deleted'), //
    ('10411', 'Serial port mode mismatch'), //
    ('10412', 'Trajectory does not exist'), //
    ('10413', 'Number of path trajectory points of patterns is full'), //
    ('10414', 'PTZ in use'), //
    ('10415', 'Prepositioning in Watch'), //
    ('10416', 'Cruise Path Used in Watch'), //

    ('10799', 'Maximum error code of PTZ module'), //
    ('10800', 'Playback abnormal end'), //
    ('10801', 'Playback end'), //
    ('10802', 'Failed to call driver interface'), //
    ('10803', 'Control block not found'), //
    ('10804', 'Exceeding playback capability'), //
    ('10805', 'The number of labels exceeds the limit'), //
    ('10806', 'No recording'), //
    ('10807', 'GB device does not support smart playback'), //
    ('10808', 'url   Cannot get the URL for playback'), //

    ('10999', 'Maximum error code of playback module'), //
    ('101000', 'Storage exception termination'), //
    ('11001', 'Array already exists'), //
    ('11002', 'Non-array mode'), //
    ('11003', 'The channel is not equipped with a video plan.'), //
    ('11022', 'No hard disk or hard disk abnormalities'), //
    ('11023', '1 Slot 1 disk is out of order'), //
    ('11024', '1 After opening the face, slot 1 is pulled out and inserted. It needs to be restarted.'),
    //
    ('11034', 'Abnormal battery status of equipment'), //

    ('11199', 'Maximum error code of storage module'), //

    // Error Codes of the Storage Module
    ('101200', 'Users are not online'), //
    ('11201', 'User password error'), //
    ('11202', 'No user'), //
    ('11203', 'User has no privilege'), //
    ('11204', 'User cap - cannot be added'), //
    ('11205', 'User already exists'), //
    ('11206', 'The number of users logged in has reached the upper limit.'), //
    ('11207', 'The user has been locked'), //
    ('11208', 'This operation is not allowed. The user is the system administrator.'), //
    ('11209', 'Not within the validity period'), //

    // Error Codes of the User Module
    ('11210', 'The number of user roles exceeds the upper limit'), //
    ('11211', 'User security logon lock'), //
    ('11212', 'Roles already exist'), //
    ('11213', 'Not in the valid period'), //
    ('11214', 'Logon failed. Please try remote login again after changing the local network to a strong password.'),
    //
    ('11217', 'Users log in with temporary passwords'), //
    ('11218', 'Temporary password of public network prohibits landing'), //
    ('11219', 'Force password modification'), //
    ('11221', 'IP Login IP has no permission'), //

    ('11599', 'Maximum error code of user module'), //

    // Error Codes of Device Management Module
    ('11600', 'The device is not online'), //
    ('11601', 'Device password error'), //
    ('11602', 'Repetitive equipment'), //
    ('11603', 'No equipment'), //
    ('11604', 'No channel'), //
    ('11605', 'VMS equipment upper limit - Can not be added anymore'), //
    ('11606', 'Upper limit of device logon number'), //
    ('11607', 'Common errors returned by devices'), //
    ('11608', 'ID No device history ID exists'), //
    ('11609', 'Devices are not allowed to be deleted'), //
    ('11610', 'Channel not online'), //
    ('11611', 'Mismatch of device type added'), //
    ('11612', 'Duplicate device name'), //
    ('11613', 'Device configuration failure'), //
    ('11614', 'Inquiries without results'), //
    ('11617', '116151161611617 Decoding equipment offline 11615, 11616 error code integration machine has been used, so the new 11617'),

    ('11661', 'VCA Failed to get VCA alarm picture'), //

    ('11801', 'Failed to read file (directory) status'), //
    ('11802', 'File does not exist'), //
    ('11803', 'Failed to create directory'), //
    ('11804', 'This user is subscription is full and the error code of the device side user is subscription is full cannot be returned.'),

    ('11805', 'admin Non-admin users cannot upgrade'), //
    ('11806', 'Upgrade not started'), //
    ('11807', 'Upgrade processing'), //
    ('11808', 'Insufficient upgrade memory space'), //
    ('11809', 'Upgrade error opening mirror file'), //
    ('11810', 'FLASH Error upgrading FLASH'), //
    ('11811', 'Multiple upgrade processes cannot be loaded at the same time'), //
    ('11812', 'Upgrade timeout'), //
    ('11813', 'Invalid configuration file'), //
    ('11814', 'Unallocated storage resources'), //

    ('11999', 'Maximum error code of equipment module'), //

    // Error Codes of the Organization Management Module
    ('12000', 'Organizational cap - cannot be added'), //
    ('12001', 'Upper limit of organizational hierarchy'), //
    ('12002', 'Organizational resources - cannot be deleted'), //
    ('12003', 'Organizations already exist under the same branch'), //
    ('12004', 'Organization does not exist'), //
    ('12005', 'This operation is not allowed. The organization is the root organization.'), //
    ('12006', 'Paternal organization does not exist'), //
    ('12007', 'Organized, not deleted'), //

    ('12399', 'Maximum error code value of organization module'), //

    // Error Codes of the Video Wall Management Module
    ('12400', 'The number of windows exceeds the limit'), //
    ('12401', 'The number of TV walls exceeds the limit'), //
    ('12402', 'LED The number of virtual LEDs exceeds the limit'), //
    ('12403', 'LED Single virtual LED line number exceeding limit'), //
    ('12404', 'The number of scenes of single TV Wall exceeds the limit'), //
    ('12405', 'The number of base diagrams that the device can store exceeds the limit'), //
    ('12406', 'The number of backgrounds that a single scene can bind exceeds the limit'), //
    ('12407', 'Destination position super-decoding capability'), //
    ('12408', 'The area of window opening has exceeded three times the area of window opening.'), //
    ('12409', 'Window position/roaming position out of range'), //
    ('12410', 'LED The number of Rolling virtual LEDs exceeds the limit'), //

    ('12430', 'The current window has been locked'), //
    ('12431', 'LED Virtual LED does not exist'), //
    ('12432', 'Rotary cutting already exists'), //
    ('12433', 'Window does not exist'), //
    ('12434', 'The window already exists'), //
    ('12435', 'The split screen in the window does not exist'), //
    ('12436', 'Windows cannot be split'), //
    ('12437', 'Decoding channel occupied by other TV wall'), //
    ('12438', 'TV Wall Existence Window'), //
    ('12439', 'TV wall information does not exist'), //
    ('12440', 'Error splitting type'), //
    ('12411', 'Error in channel number'), //
    ('12442', 'Media streaming is not ready'), //
    ('12443', 'VO The number of windows exceeds the limit'), //

    // Network Related Error Codes
    ('12800', 'IP IP Address Conflict'), //
    ('12801', 'The mapped port is discarded'), //
    ('12802', 'Port occupied'), //
    ('12803', 'VMP Failed to set VMP port'), //
    ('12804', 'Failed to set media stream port'), //
    ('12805', 'RTSP Failed to set RTSP port'), //
    ('12806', 'ONVIF Failed to set ONVIF port'), //
    ('12807', 'HTTPS Failed to set HTTPS port'), //
    ('12808', 'HTTP Failed to set HTTP port'), //
    ('12809', 'Failed to send test mail'), //
    ('12810', 'Initialize default values'), //
    ('12811', 'Update completed'), //
    ('12812', ',ip The update is complete, but the IP address remains unchanged'), //
    ('12813', 'Error username password'), //
    ('12814', 'offline URL Special options require service fees similar to offline URLs'), //
    ('12815', 'Host name format is not qualified'), //
    ('12816', 'The hostname account does not exist'), //
    ('12817', '20 More than 20 hosts are requested to update immediately'), //
    ('12818', 'Host name is disabled due to abuse'), //
    ('12819', 'HTTP The proxy sends the wrong HTTP request format'), //
    ('12820', '127.0.0.1http Unless the client wants to update 127.0.0.1, this information indicates an error in the HTTP request specification'),

    ('12821', 'DNSSupport DNS error, please contact Support'), //
    ('12822', 'Service maintenance is being processed'), //
    ('12823', 'DDNS DDNS request failed to send'), //
    ('12824', 'Check code error'), //
    ('12825', 'Domain name format is not qualified'), //
    ('12826', 'Domain name conflict'), //
    ('12827', 'ddnsddns Non-ddns server returns, only for closing DDNS record status'), //
    ('12828', 'Certificate file generation failed'), //
    ('12829', 'It is illegal to upload certificate files'), //
    ('12830', 'Error in username or password of authentication server'), //
    ('12831', 'Mail delivery failed. Check network and mail configuration'), //
    ('12832', 'Mail Delivery Timeout'), //
    ('12833', 'The cloud account is logged in'), //
    ('12834', 'Failed to add cloud device information'), //
    ('12835', 'Cloud organizational information is empty'), //
    ('12836', 'Cloud information cannot be obtained. Check network configuration'), //
    ('12837', 'Cloud account does not exist'), //

    ('13199', 'Maximum Error Code Value of Network Module'), //

    ('13200', 'Illegal configuration file'), //
    ('13201', 'XML Failed to export XML file'), //
    ('13202', 'XML Failed to generate XML file'), //
    ('13203', 'XML XML attribute information does not exist'), //
    ('13204', 'Configuration area synchronization error to save configuration synchronously when system restart'),

    ('13205', 'Simple Recovery Configuration Error'), //
    ('13206', 'Complete Recovery Configuration Error'), //
    ('13207', 'Disable Writing Configuration'), //

    ('13599', 'XML Maximum Error Code Value of XML Module'), //

    ('13600', 'Template is being used'), //
    ('13601', 'Time template node does not exist'), //
    ('13602', 'Illegal Holiday Time'), //
    ('13603', 'No plan'), //
    ('13604', 'Not planned'), //
    ('13605', 'Within the plan'), //

    ('13699', 'Plan Maximum Error Code Value of Plan Module'), //

    ('13700', 'Equipment upgrade, version number mismatch'), //
    ('13701', 'Equipment upgrade, equipment type mismatch'), //
    ('13702', 'Document Check Error'), //
    ('13703', 'Export configuration error'), //
    ('13704', 'Import configuration error'), //
    ('13705', 'Device upgrade, memory application or other reasons may fail'), //
    ('13706', 'Maintenance information is being exported'), //

    ('13799', 'MT Maximum error code value of MT module'), //

    ('13800', 'Preventive clothing plan already exists'), //
    ('13801', 'The designated deployment plan does not exist'), //
    ('13802', 'The specified linkage rule does not exist'), //

    ('13899', 'Linkage Maximum Error Code Value of Linkage Module'), //

    ('13900', 'Alarm suppression'), //

    ('15000', 'The number of pictures reached the upper limit and could not be added.'), //
    ('15001', 'The root directory already exists in the electronic map'), //

    ('15099', 'Maximum Error Code Value of Electronic Map Module'), //

    ('17000', 'No entrance guards exist'), //
    ('17001', 'Access guards already exist'), //
    ('17002', 'Access card already exists'), //
    ('17003', 'No access card exists'), //
    ('17004', 'The number of access cards has reached the upper limit.'), //
    ('17005', 'Personnel access records do not exist'), //
    ('17006', 'Access control channel does not exist'), //
    ('17007', 'Personnel with access control authority do not exist'), //
    ('17008', 'ID Personnel Departmental ID of Access Control Personnel does not exist'), //
    ('17009', 'ID Personnel access record ID does not exist'), //
    ('17010', 'Departmental authority already exists'), //
    ('17011', 'The permission group has Person and cannot be deleted.'), //
    ('17012', 'Existing blacklist Persons .'), //
    ('17013', 'Blacklist visitors cannot be added.'), //
    ('17999', 'PACS Maximum Error Code of PACS Module'), //

    ('19000', 'No member in face database'), //
    ('19001', 'Control task does not exist'), //
    ('19002', 'There is no alarm record for face recognition'), //
    ('19003', 'Failure in capturing face images'), //
    ('19004', 'Uid No Uid is available'), //

    ('19005', 'Maximum number of face databases'), //
    ('19006', 'The number of control tasks in the face database has reached the maximum.'), //
    ('19007', 'The maximum number of face members has been achieved in this face database.'), //
    ('19008', 'Document number already exists'), //
    ('19009', 'Version mismatch of device face recognition algorithm'), //
    ('19010', 'Vehicle control tasks do not exist'), //
    ('19011', 'Failure to obtain license plate image'), //
    ('19012', 'Vehicle records do not exist'), //
    ('19013', 'Failed to get slave picture'), //
    ('19014', 'Out-of-line status of view Library of distribution control equipment'), //
    ('19015', 'In Distribution Control Equipment'), //
    ('19020', 'This control task is not enabled'), //
    ('19021', 'In charge control'), //
    ('19022', 'GUARD_DIFFERENT_IMAGE_PROTOCOL '), //
    ('19023', ' ROOT_LIB_HAS_MONITORED_MEMBER '), //

    ('19999', 'Maximum Error Code of Intelligent Module'), //

    ('25006', 'View library layout control error, equipment busy '), //
    ('25007', 'View database layout control error, face database has reached the upper limit '), //
    ('25008', 'View database layout control error, face members have reached the upper limit '), //
    ('25009', 'View Library layout control error, Device Smart Bar does not exist '), //
    ('25010', 'View Library layout error, Device View Library Not Connected '), //

    // Cloud Service Related Error Codes
    ('20000', 'Not your Device'), //
    ('20001', 'Channel does not exist'), //
    ('20002', 'ID Flow ID does not exist'), //
    ('20003', 'Excessive request rate '), //
    ('20004', 'ID Live ID does not exist '), //
    ('20005', 'request Flow Protocol does not support '), //
    ('20006', 'ID Client ID does not exist '), //
    ('20007', 'RequestID RequestID does not exist'), //
    ('20008', 'Too many Current online users '), //
    ('20009', 'RTMPURL failed to obtain RTMP pull URL '), //
    ('20010', 'Failure to acquire push - Flow status '), //
    ('20011', 'Start - up pull -out Failure '), //
    ('20012', 'failed to get the list of cloud devices '), //
    ('20013', 'failed to obtain cloud Device information '), //
    ('20014', 'NAT failed to get NAT type '), //
    ('20015', ' failed to update cloud Device name '), //
    ('20016', 'failed to delete cloud devices '), //
    ('20017', 'Stop pulling and lose '), //
    ('20018', 'Failure of Pushing Flow to Preserve Life '), //
    ('20019', 'the Live list is empty '), //
    ('20020', 'ID failed to get Live ID '), //
    ('20021', 'Illegal format '), //
    ('20022', 'the Device is not registered '), //
    ('20023', 'Registration code does not exist '), //
    ('20024', 'Can not share with oneself '), //
    ('20025', 'failed to add cloud devices '), //

    ('20026', 'DDNS Failed to obtain DDNS information'), //
    ('20027', 'Failed to retrieve cloud organization list'), //
    ('20028', 'Failed to get shared device list'), //
    ('20029', 'Failure to start device sharing'), //
    ('20030', 'Failure to Stop Cloud Device Sharing'), //
    ('20031', 'Failed to obtain cloud shared record information'), //
    ('20032', 'Failed to obtain cloud account device statistics'), //
    ('20033', 'Increasing cloud Organizational Failure'), //
    ('20034', 'Failure to modify cloud organization'), //
    ('20035', 'Failed to delete cloud organizations'), //
    ('20036', 'Failure to add devices to cloud organizations'), //
    ('20037', 'Failure to access cloud device channel'), //
    ('20038', 'Failed to get cloud video time information'), //
    ('20039', 'M3u8 Failed to get cloud video M3u8 information') //

    );

type
  TPlayerNetDEV = class;

  TNETDEVAlarmInfo = record
    tAlarmTime: Int64;
    dwChannelID: Int32;
    wIndex: UINT16;
    pszName: PAnsiChar;
    dwTotalBandWidth: Int32;
    dwUnusedBandwidth: Int32;
    dwTotalStreamNum: Int32;
    dwFreeStreamNum: Int32;
    byRes: Array [0 .. 10 - 1] of BYTE;
  end;

  TSourceDataCallBack = procedure(lpRealHandle: IntPtr; var pucBuffer: BYTE; dwBufSize: Int32;
    dwMediaDataType: Int32; lpUserParam: IntPtr)stdcall;

  TNETDEV_ExceptionCallBack_PF = procedure(plUserID: IntPtr; dwType: Int32; plExpHandle: IntPtr;
    plUserData: IntPtr)stdcall;

  TNetDEVPictureFluency = ( //
    NETDEV_PICTURE_REAL = 0, //
    NETDEV_PICTURE_FLUENCY = 1, //
    NETDEV_PICTURE_BALANCE_NEW = 3, //
    NETDEV_PICTURE_RTMP_FLUENCY = 4, //
    NETDEV_PICTURE_FLUENCY_INVALID = $FF);

  TNetDEVStreamMode = ( //
    NETDEV_STREAM_MODE_ALL = 0, //
    NETDEV_STREAM_MODE_VIDEO = $8000 //
    );

  TNetDEVPullStreamMode = ( //
    NETDEV_STREAM_MODE_SERVER_DISTRIBUTE = 0, //
    NETDEV_STREAM_MODE_FORCE_CDN = 1 //
    );

  TNetDEVDistributeCloudSrv = ( //
    NETDEV_DISTRIBUTE_CLOUD_NONE = 0, //
    NETDEV_DISTRIBUTE_CLOUD_KS = 1, //
    NETDEV_DISTRIBUTE_CLOUD_UN = 2, //
    NETDEV_DISTRIBUTE_CLOUD_TEN = 3 //
    );
  TNetDEVTransType = (NETDEV_TRANS_TYPE_FORWORD = 0, NETDEV_TRANS_TYPE_STRAIGHT = 1);

  TNETDEVException = (NETDEV_EXCEPTION_REPORT_VOD_END = 300, NETDEV_EXCEPTION_REPORT_VOD_ABEND = 301,
    NETDEV_EXCEPTION_REPORT_BACKUP_END = 302, NETDEV_EXCEPTION_REPORT_BACKUP_DISC_OUT = 303,
    NETDEV_EXCEPTION_REPORT_BACKUP_DISC_FULL = 304, NETDEV_EXCEPTION_REPORT_BACKUP_ABEND = 305,
    NETDEV_EXCEPTION_EXCHANGE = $8000, NETDEV_EXCEPTION_REPORT_INVALID = $FFFF);

  TNetDEVDeviceInfo = record
    dwDevType: Int32;
    wAlarmInPortNum: Int16;
    wAlarmOutPortNum: Int16;
    dwChannelNum: Int32;
    byRes: array [0 .. 47] of BYTE;
  end;

  TPreviewInfo = record
    dwChannelID: Int32;
    dwStreamType: Int32;
    dwLinkMode: Int32;
    hPlayWnd: HWND;
    dwFluency: Int32;
    dwStreamMode: Int32;
    dwLiveMode: Int32;
    dwDisTributeCloud: Int32;
    dwallowDistribution: BOOL;
    dwTransType: Int32;
    dwStreamProtocol: Int32;
    byRes: array [0 .. NETDEV_LEN_235] of BYTE;

    constructor Create(AHandle: HWND; AChannel: Int32 = DEFAULT_CHANNEL;
      AType: Int32 = NETSDK_LIVE_STREAM_INDEX_MAIN; AMode: Int32 = NETSDK_TRANSPROTOCOL_RTPTCP;
      AFluncy: TNetDEVPictureFluency = NETDEV_PICTURE_REAL);
  end;

  TNETDEVVideoStreamInfo = record
    enStreamType: Int32;
    bEnableFlag: Int32;
    dwHeight: Int32;
    dwWidth: Int32;
    dwFrameRate: Int32;
    dwBitRate: Int32;
    enCodeType: Int32;
    enQuality: Int32;
    dwGop: Int32;
    byRes: Array [0 .. 31] of BYTE;
  end;

  TNETDEVDeviceBasicInfo = record
    szDevModel: Array [0 .. NETDEV_LEN_64] of AnsiChar;
    szSerialNum: Array [0 .. NETDEV_LEN_64] of AnsiChar;
    szFirmwareVersion: Array [0 .. NETDEV_LEN_64] of AnsiChar;
    szMacAddress: Array [0 .. NETDEV_LEN_64] of AnsiChar;
    szDeviceName: Array [0 .. NETDEV_LEN_64] of AnsiChar;
    byRes: Array [0 .. 447] of BYTE;
  end;

  {
    CHAR    szIPAddr[NETDEV_LEN_260];       /* IP/ */
    INT32   dwPort;
    CHAR    szUserName[NETDEV_LEN_132];
    CHAR    szPassword[NETDEV_LEN_128];
    INT32   dwLoginProto;                   /* ,  NETDEV_LOGIN_PROTO_E */
    INT32   dwDeviceType;                   /* ,  NETDEV_DEVICE_TYPE_E */
    BYTE    byRes[256];
  }
  TNETDEVLoginInfo = record
    szIPAddr: Array [0 .. NETDEV_LEN_260] of AnsiChar;
    dwPort: Int32;
    szUserName: Array [0 .. NETDEV_LEN_132] of AnsiChar;
    szPassword: Array [0 .. NETDEV_LEN_128] of AnsiChar;
    dwLoginProto: Int32;
    dwDeviceType: Int32;
    byRest: Array [0 .. NETDEV_LEN_256] of AnsiChar;

    constructor Create(AInfo: TCCTVInfo);
  end;

  {
    INT32  dwSELogCount;
    INT32  dwSELogTime;
    BYTE   byRes[64];
  }
  TNETDEVSelogInfo = record
    dwSELogCount: Int32;
    dwSELogTime: Int32;
    byRes: Array [0 .. NETDEV_LEN_64] of BYTE;
  end;

  TNETDEVVideoCHLDetailInfo = record
    dwChannelID: Int32;
    bPtzSupported: BOOL;
    enStatus: Int32; // 0:OFFLINE, 1:ONLINE, 2:UNBIND
    dwStreamNum: Int32;
    szChnName: Array [0 .. NETDEV_LEN_64] of AnsiChar;
    byRes: Array [0 .. 3] of BYTE;
  end;

  TNETDEVStreamDataCB = record
    bDecode: BOOL;
    dwCBType: Int32;
    lpVideoDataCB: IntPtr;
    lpAudioDataCB: IntPtr;
    lpUserData: IntPtr;
    byRes: Array [0 .. 511] of BYTE;
  end;

  TNETDEV_REV_TIMEOUT = record
    dwRevTimeOut: Int32;
    dwFileReportTimeOut: Int32;
    byRes: Array [0 .. 127] of BYTE;
  end;

  TNETDEV_Init = function: Boolean stdcall;
  TNETDEV_Login = function(pszDevIP: PAnsiChar; wDevPort: Int16; pszUserName: PAnsiChar;
    pszPassword: PAnsiChar; var pstDevInfo: TNetDEVDeviceInfo): IntPtr stdcall;
  TNETDEV_Login_V30 = function(var pstDevLoginInfo: TNETDEVLoginInfo; var pstSELogInfo: TNETDEVSelogInfo)
    : IntPtr stdcall;
  TNETDEV_QueryVideoChlDetailList = function(lpUserID: IntPtr; var pdwChlCount: IntPtr;
    var pstVideoChlList: TNETDEVVideoCHLDetailInfo): IntPtr stdcall;

  TNETDEV_Logout = function(lpUserID: IntPtr): Boolean stdcall;
  TNETDEV_GetDevConfig = function(lpUserID: IntPtr; dwChannelID: Int32; dwCommand: Int32;
    var lpOutBuffer: TNETDEVVideoStreamInfo; dwOutBufferSize: Int32; var pdwBytesReturned: Int32)
    : Boolean stdcall;
  TNETDEV_RealPlay = function(lpUserID: IntPtr; var pstPreviewInfo: TPreviewInfo;
    cbDataCallBack: TSourceDataCallBack; lpUserData: IntPtr): IntPtr stdcall;
  TNETDEV_RealPlay_V30 = function(lpUserID: IntPtr; var pstPreviewInfo: TPreviewInfo;
    lpStreamDataCB: TNETDEVStreamDataCB): IntPtr stdcall;

  TNETDEV_StopRealPlay = function(lpRealHandle: IntPtr): Boolean stdcall;
  TNETDEV_CapturePicture = function(lpRealHandle: IntPtr; szFileName: PAnsiChar; dwCaptureMode: Int32)
    : Boolean stdcall;
  TNETDEV_GetLastError = function(): Int32 stdcall;
  TNETDEV_SetExceptionCallBack = function(cbExceptionCallBack: TNETDEV_ExceptionCallBack_PF;
    lpUserData: IntPtr): Boolean stdcall;
  TNETDEV_Cleanup = function(): Boolean stdcall;
  TNETDEV_SetLogPath = function(pszLogPath: PAnsiChar): Boolean stdcall;

  TNETDEV_GetStreamUrl = function(lpUserID: IntPtr; dwChannelID: Int32; dwStreamType: Int32;
    pszStreamUrl: PAnsiChar): Boolean stdcall;
  TNETDEV_FastRealPlayByUrl = function(lpUserID: IntPtr; pszUrl: PAnsiChar; var pstPreviewInfo: TPreviewInfo;
    cbDataCallBack: TSourceDataCallBack; lpUserData: IntPtr): IntPtr stdcall;
  TNETDEV_FastRealPlayByUrl_V30 = function(lpUserID: IntPtr; pszUrl: PAnsiChar; pszUserName: PAnsiChar;
    pszPassword: PAnsiChar; var pstPreviewInfo: TPreviewInfo; cbDataCallBack: TSourceDataCallBack;
    lpUserData: IntPtr): IntPtr stdcall;
  TNETDEV_SetConnectTime = function(dwWaitTime: Int32; dwTrytimes: Int32): Boolean stdcall;
  TNETDEV_SetPlayDataCallBack = function(lpRealHandle: IntPtr; cbPlayDataCallBack: IntPtr; bContinue: Int32;
    lpUserData: IntPtr): Boolean stdcall;
  TNETDEV_SetRevTimeOut = function(var pstRevTimeout: TNETDEV_REV_TIMEOUT): Boolean stdcall;

  TPlayerNetDEV = class(TPlayerAbstract)
  protected
    function _StopRealPlay: Boolean; override;
    function _Logout: Boolean; override;
    function _RealPlay: IntPtr; override;
  public
    function Login: Boolean; override;
    function CapturePicture(const szFileName: String): Boolean; override;
  public
    function GetStreamInfo(const dwChannelID: Int32; var lpOutBuffer: TNETDEVVideoStreamInfo;
      const dwOutBufferSize: Int32; var pdwBytesReturned: Int32): Boolean;
    function GetStreamUrl(AChannel: Int32 = DEFAULT_CHANNEL; AType: Int32 = NETSDK_LIVE_STREAM_INDEX_MAIN)
      : PAnsiChar;
    function RealPlayByUrl(_url: PAnsiChar; AControl: TWinControl; AChannel: Int32 = DEFAULT_CHANNEL;
      AStreamType: Int32 = NETSDK_LIVE_STREAM_INDEX_MAIN): Boolean;
    function RealPlayByUrl_V30(_url: PAnsiChar; _id: PAnsiChar; _pwd: PAnsiChar; AControl: TWinControl;
      AChannel: Int32 = DEFAULT_CHANNEL; AStreamType: Int32 = NETSDK_LIVE_STREAM_INDEX_MAIN): Boolean;

    function GetLastError: String; override;
  end;

  TNetDEV = class(TNetAbstract)
  protected
    procedure LoadDLL(const APath: string); override;
    function InitLib: Boolean; override;
    function SetLogPath(const szPath: string): Boolean; override;
    function CreatePlayer(const AID: string): TPlayerAbstract; override;
  public
    function GetLastError: string; override;
    function Cleanup: Boolean; override;

    function SetConnectTime(dwWaitTime: Int32 = 10; dwTrytimes: Int32 = 30): Boolean;
    function SetRevTimeOut(dwRevTimeOut: Int32 = 5; dwFileReportTimeOut: Int32 = 30): Boolean;
  end;

implementation

var
  FNETDEV_Init: TNETDEV_Init;
  FNETDEV_StopRealPlay: TNETDEV_StopRealPlay;
  FNETDEV_SetExceptionCallBack: TNETDEV_SetExceptionCallBack;
  FNETDEV_CapturePicture: TNETDEV_CapturePicture;
  FNETDEV_RealPlay: TNETDEV_RealPlay;
  FNETDEV_RealPlay_V30: TNETDEV_RealPlay_V30;
  FNETDEV_SetPlayDataCallBack: TNETDEV_SetPlayDataCallBack;
  FNETDEV_Logout: TNETDEV_Logout;
  FNETDEV_GetDevConfig: TNETDEV_GetDevConfig;
  FNETDEV_Login: TNETDEV_Login;
  FNETDEV_QueryVideoChlDetailList: TNETDEV_QueryVideoChlDetailList;
  FNETDEV_Login_V30: TNETDEV_Login_V30;
  FNETDEV_Cleanup: TNETDEV_Cleanup;
  FNETDEV_SetLogPath: TNETDEV_SetLogPath;

  FNETDEV_GetStreamUrl: TNETDEV_GetStreamUrl;
  FNETDEV_FastRealPlayByUrl: TNETDEV_FastRealPlayByUrl;
  FNETDEV_FastRealPlayByUrl_V30: TNETDEV_FastRealPlayByUrl_V30;
  FNETDEV_SetConnectTime: TNETDEV_SetConnectTime;

  FNETDEV_GetLastError: TNETDEV_GetLastError;
  FNETDEV_SetRevTimeOut: TNETDEV_SetRevTimeOut;

  retIntPtr: IntPtr;

function StreamCompare(Stream1, Stream2: TStream): Boolean;
var
  Data1, Data2: UInt64;
begin
  Result := False;
  if Stream1.Size <> Stream2.Size then
    Exit;

  Stream1.Position := 0;
  Stream2.Position := 0;
  while Stream1.Position + SizeOf(Data1) < Stream1.Size do
  begin
    Stream1.ReadData(Data1);
    Stream2.ReadData(Data2);
    if Data1 <> Data2 then
      Exit;
  end;
  Result := True;
end;

procedure OnSourceDataCallBack(lpRealHandle: IntPtr; var pucBuffer: BYTE; dwBufSize, dwMediaDataType: Int32;
  lpUserParam: IntPtr)stdcall;
var
  Player: TPlayerNetDEV;
begin
  Player := Pointer(lpUserParam);
  Player.Tick := GetTickCount;
end;

function GetErrorMessage(const ACode: Int32): string;
var
  Msg: string;
  I: Int32;
begin
  Msg := 'Unknown Error';
  for I := Low(ERROR_CODE) to High(ERROR_CODE) do
  begin
    if ERROR_CODE[I, 0].ToInteger = ACode then
      Msg := ERROR_CODE[I, 1];
  end;
  Result := format('%s(%d)', [Msg, ACode]);
end;

{ TNetDEV }

function TNetDEV.Cleanup: Boolean;
var
  MyPlayer: TPlayerAbstract;
begin
  if not FInited then
    Exit(False);

  for MyPlayer in FPlayerDic.Values do
  begin
    MyPlayer.StopRealPlay;
    MyPlayer.Logout;
  end;

  Result := FNETDEV_Cleanup;
  if Result then
  begin
    FInited := False;
    TLogging.Obj.ApplicationMessage(msDebug, 'ND_Cleanup')
  end
  else
    TLogging.Obj.ApplicationMessage(msWarning, 'ND_Cleanup', GetLastError)
end;

function TNetDEV.InitLib: Boolean;
begin
  Result := FNETDEV_Init;
end;

function TNetDEV.CreatePlayer(const AID: string): TPlayerAbstract;
begin
  Result := TPlayerNetDEV.Create(AID);
end;

function TNetDEV.GetLastError: String;
begin
  if FDLLHandle = 0 then
    Exit('Unload DLL');

  Result := GetErrorMessage(FNETDEV_GetLastError);
end;

procedure TNetDEV.LoadDLL(const APath: string);
begin
  SetCurrentDir(APath);
  FDLLHandle := LoadLibrary(NETDEVSDK_DLL);
  if FDLLHandle < 32 then
    raise Exception.Create('Load DLL Exception');

  @FNETDEV_Init := GetProcAddress(FDLLHandle, 'NETDEV_Init');
  @FNETDEV_Login := GetProcAddress(FDLLHandle, 'NETDEV_Login');
  @FNETDEV_Login_V30 := GetProcAddress(FDLLHandle, 'NETDEV_Login_V30');
  @FNETDEV_QueryVideoChlDetailList := GetProcAddress(FDLLHandle, 'NETDEV_QueryVideoChlDetailList');
  @FNETDEV_Logout := GetProcAddress(FDLLHandle, 'NETDEV_Logout');
  @FNETDEV_RealPlay := GetProcAddress(FDLLHandle, 'NETDEV_RealPlay');
  @FNETDEV_RealPlay_V30 := GetProcAddress(FDLLHandle, 'NETDEV_RealPlay_V30');
  @FNETDEV_GetDevConfig := GetProcAddress(FDLLHandle, 'NETDEV_GetDevConfig');

  @FNETDEV_StopRealPlay := GetProcAddress(FDLLHandle, 'NETDEV_StopRealPlay');
  @FNETDEV_CapturePicture := GetProcAddress(FDLLHandle, 'NETDEV_CapturePicture');
  @FNETDEV_GetLastError := GetProcAddress(FDLLHandle, 'NETDEV_GetLastError');
  @FNETDEV_SetExceptionCallBack := GetProcAddress(FDLLHandle, 'NETDEV_SetExceptionCallBack');
  @FNETDEV_SetPlayDataCallBack := GetProcAddress(FDLLHandle, 'NETDEV_SetPlayDataCallBack');
  @FNETDEV_Cleanup := GetProcAddress(FDLLHandle, 'NETDEV_Cleanup');
  @FNETDEV_SetLogPath := GetProcAddress(FDLLHandle, 'NETDEV_SetLogPath');
  @FNETDEV_GetStreamUrl := GetProcAddress(FDLLHandle, 'NETDEV_GetStreamUrl');
  @FNETDEV_FastRealPlayByUrl := GetProcAddress(FDLLHandle, 'NETDEV_FastRealPlayByUrl');
  @FNETDEV_FastRealPlayByUrl_V30 := GetProcAddress(FDLLHandle, 'NETDEV_FastRealPlayByUrl_V30');
  @FNETDEV_SetConnectTime := GetProcAddress(FDLLHandle, 'NETDEV_SetConnectTime');
  @FNETDEV_SetRevTimeOut := GetProcAddress(FDLLHandle, 'NETDEV_SetRevTimeOut');

  SetCurrentDir(TPath.GetDocumentsPath);
  TLogging.Obj.ApplicationMessage(msDebug, 'ND_LoadDLL', 'Path=%s,Handle=%d', [APath, FDLLHandle]);
end;

function TNetDEV.SetConnectTime(dwWaitTime, dwTrytimes: Int32): Boolean;
begin
  Result := FNETDEV_SetConnectTime(dwWaitTime, dwTrytimes);
  if Result then
    TLogging.Obj.ApplicationMessage(msWarning, 'ND_SetConnectTime', 'WaitTime=%d,TryTimes=%d',
      [dwWaitTime, dwTrytimes])
  else
    TLogging.Obj.ApplicationMessage(msWarning, 'ND_SetConnectTime', 'E=%s', [GetLastError]);
end;

function TNetDEV.SetLogPath(const szPath: string): Boolean;
var
  path: AnsiString;
begin
  path := AnsiString(szPath);
  Result := FNETDEV_SetLogPath(PAnsiChar(path));
  if Result then
    TLogging.Obj.ApplicationMessage(msDebug, 'ND_SetLogPath', 'Path=%s', [szPath])
  else
    TLogging.Obj.ApplicationMessage(msWarning, 'ND_SetLogPath', 'E=%s', [GetLastError]);
end;

function TNetDEV.SetRevTimeOut(dwRevTimeOut, dwFileReportTimeOut: Int32): Boolean;
var
  rev: TNETDEV_REV_TIMEOUT;
begin
  rev.dwRevTimeOut := dwRevTimeOut;
  rev.dwFileReportTimeOut := dwFileReportTimeOut;
  FillChar(rev.byRes, NETDEV_LEN_128, #0);
  Result := FNETDEV_SetRevTimeOut(rev);
  if Result then
    TLogging.Obj.ApplicationMessage(msDebug, 'ND_SetRevTimeOut', 'RevTimeOut=%d,FileReportTimeOut=%d',
      [dwRevTimeOut, dwFileReportTimeOut])
  else
    TLogging.Obj.ApplicationMessage(msWarning, 'ND_SetRevTimeOut', 'E=%s', [GetLastError]);
end;

{ TPreviewInfo }

constructor TPreviewInfo.Create(AHandle: HWND; AChannel: Int32; AType: Int32; AMode: Int32;
  AFluncy: TNetDEVPictureFluency);
begin
  Self.dwChannelID := AChannel;
  Self.dwStreamType := AType;
  Self.dwLinkMode := AMode;
  Self.hPlayWnd := AHandle;
  Self.dwFluency := Int32(AFluncy);
  Self.dwStreamMode := Int32(TNetDEVStreamMode.NETDEV_STREAM_MODE_VIDEO);
  Self.dwLiveMode := Int32(TNetDEVPullStreamMode.NETDEV_STREAM_MODE_SERVER_DISTRIBUTE);
  Self.dwDisTributeCloud := Int32(TNetDEVDistributeCloudSrv.NETDEV_DISTRIBUTE_CLOUD_NONE);
  Self.dwallowDistribution := False;
  Self.dwTransType := 0;
  Self.dwStreamProtocol := 0;
  FillChar(Self.byRes, NETDEV_LEN_235, #0);
end;

{ TPlayerInfo }

function TPlayerNetDEV.CapturePicture(const szFileName: String): Boolean;
var
  Name: AnsiString;
begin
  if FRealHandle = 0 then
    raise Exception.Create('StopRealPlay');

  Name := AnsiString(szFileName);
  Result := FNETDEV_CapturePicture(FRealHandle, PAnsiChar(Name), NETDEV_PICTURE_JPG);
end;

function TPlayerNetDEV.GetLastError: String;
begin
  Result := GetErrorMessage(FNETDEV_GetLastError);
end;

function TPlayerNetDEV.GetStreamInfo(const dwChannelID: Int32; var lpOutBuffer: TNETDEVVideoStreamInfo;
  const dwOutBufferSize: Int32; var pdwBytesReturned: Int32): Boolean;
const
  NETDEV_GET_STREAMCFG = 120;
begin
  if FUserID = 0 then
    Exit(False);
  Result := FNETDEV_GetDevConfig(FUserID, dwChannelID, NETDEV_GET_STREAMCFG, lpOutBuffer, dwOutBufferSize,
    pdwBytesReturned);
end;

function TPlayerNetDEV.GetStreamUrl(AChannel: Int32; AType: Int32): PAnsiChar;
var
  Url: PAnsiChar;
begin
  Url := AllocMem(300);
  if FNETDEV_GetStreamUrl(FUserID, AChannel, AType, Url) then
    TLogging.Obj.ApplicationMessage(msDebug, 'ND_GetStreamUrl', '%s', [Url])
  else
    TLogging.Obj.ApplicationMessage(msWarning, 'ND_GetStreamUrl', 'E=%s', [GetLastError]);
  Result := Url;
end;

function TPlayerNetDEV.Login(): Boolean;
var
  pstDevLoginInfo: TNETDEVLoginInfo;
  pstDevSelogInfo: TNETDEVSelogInfo;
  ChCount: IntPtr;
  Info: TNETDEVVideoCHLDetailInfo;
begin
  FLoggingIn := True;
  try
    TLogging.Obj.ApplicationMessage(msDebug, 'ND_TryLogin', 'ID=%s,TryCount=%d', [FID, FRetryCount]);
    pstDevLoginInfo := TNETDEVLoginInfo.Create(FCCTVInfo);
    FUserID := FNETDEV_Login_V30(pstDevLoginInfo, pstDevSelogInfo);
  finally
    FLoggingIn := False;
  end;

  Result := Self.Logined;
  if Result then
  begin
    FLoggingIn := False;
    FRetryCount := 0;
    TLogging.Obj.ApplicationMessage(msInfo, 'ND_LoginSuccess', 'ID=%s,UserID=%d', [FID, FUserID]);
    retIntPtr := FNETDEV_QueryVideoChlDetailList(FUserID, ChCount, Info);
    TLogging.Obj.ApplicationMessage(msInfo, 'ND_ChannelList', 'ID=%s,Count=%d', [FID, ChCount]);
  end
  else
  begin
    FRetryCount := FRetryCount + 1;
    if Assigned(OnException) then
      OnException(Self, GetLastError);
  end;
end;

function TPlayerNetDEV.RealPlayByUrl(_url: PAnsiChar; AControl: TWinControl; AChannel: Int32;
  AStreamType: Int32): Boolean;
begin
  if FUserID = 0 then
    raise Exception.Create('Logout');

  if FRealHandle > 0 then
    StopRealPlay;

  ThreadSafeSync(
    procedure
    var
      PreviewInfo: TPreviewInfo;
    begin
      FPlayControl := AControl;
      FPlayControl.Invalidate;
      FChannel := AChannel;
      FStreamType := AStreamType;
      PreviewInfo := TPreviewInfo.Create(AControl.Handle, AChannel, AStreamType);
      FRealHandle := FNETDEV_FastRealPlayByUrl(FUserID, _url, PreviewInfo, OnSourceDataCallBack,
        IntPtr(Self));
    end);

  Result := Self.Played;
  if Result then
  begin
    FTick := GetTickCount;
    FRetryCount := 0;
    TLogging.Obj.ApplicationMessage(msInfo, 'NETDEV_FastRealPlayByUrl', 'ID=%s,UserID=%d,RealHandle=%d',
      [FID, FUserID, FRealHandle]);
  end
  else
  begin
    FRetryCount := FRetryCount + 1;
    TLogging.Obj.ApplicationMessage(msWarning, 'NETDEV_FastRealPlayByUrl', 'ID=%s,UserID=%d,TryCount=%d,E=%s',
      [FID, FUserID, FRetryCount, GetLastError]);
  end;
end;

function TPlayerNetDEV.RealPlayByUrl_V30(_url: PAnsiChar; _id: PAnsiChar; _pwd: PAnsiChar;
AControl: TWinControl; AChannel: Int32; AStreamType: Int32): Boolean;
begin
  if FUserID = 0 then
    raise Exception.Create('Logout');

  if FRealHandle > 0 then
    StopRealPlay;

  ThreadSafeSync(
    procedure
    var
      PreviewInfo: TPreviewInfo;
    begin
      FPlayControl := AControl;
      FPlayControl.Invalidate;
      FChannel := AChannel;
      FStreamType := AStreamType;
      PreviewInfo := TPreviewInfo.Create(AControl.Handle, AChannel, AStreamType);
      FRealHandle := FNETDEV_FastRealPlayByUrl_V30(FUserID, _url, _id, _pwd, PreviewInfo,
        OnSourceDataCallBack, IntPtr(Self));
    end);

  Result := Self.Played;
  if Result then
  begin
    FTick := GetTickCount;
    FRetryCount := 0;
    TLogging.Obj.ApplicationMessage(msInfo, 'NETDEV_FastRealPlayByUrl_V30', 'ID=%s,UserID=%d,RealHandle=%d',
      [FID, FUserID, FRealHandle]);
  end
  else
  begin
    FRetryCount := FRetryCount + 1;
    TLogging.Obj.ApplicationMessage(msWarning, 'NETDEV_FastRealPlayByUrl_V30',
      'ID=%s,UserID=%d,TryCount=%d,E=%s', [FID, FUserID, FRetryCount, GetLastError]);
  end;
end;

function TPlayerNetDEV._Logout: Boolean;
begin
  Result := FNETDEV_Logout(FUserID);
end;

function TPlayerNetDEV._RealPlay: IntPtr;
var
  PreviewInfo: TPreviewInfo;
begin
  PreviewInfo := TPreviewInfo.Create(FPlayControl.Handle, FChannel, FStreamType);
  Result := FNETDEV_RealPlay(FUserID, PreviewInfo, OnSourceDataCallBack, IntPtr(Self));
end;

function TPlayerNetDEV._StopRealPlay: Boolean;
begin
  Result := FNETDEV_StopRealPlay(FRealHandle);
end;

{ TNETDEVLoginInfo }

constructor TNETDEVLoginInfo.Create(AInfo: TCCTVInfo);
var
  tmp: AnsiString;
begin
  FillChar(Self, SizeOf(TNETDEVLoginInfo), #0);
  tmp := AnsiString(AInfo.IP);
  CopyMemory(@Self.szIPAddr[0], @tmp[1], Length(AInfo.IP));
  tmp := AnsiString(AInfo.ID);
  CopyMemory(@Self.szUserName[0], @tmp[1], Length(AInfo.ID));
  tmp := AnsiString(AInfo.Password);
  CopyMemory(@Self.szPassword[0], @tmp[1], Length(AInfo.Password));
  Self.dwPort := AInfo.Port;
  Self.dwLoginProto := NETDEV_LOGIN_PROTO_ONVIF;
  Self.dwDeviceType := 0;
end;

end.
