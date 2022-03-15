package io.choerodon.agile.app.service.impl;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.github.junrar.Archive;
import com.github.junrar.exception.RarException;
import com.github.junrar.rarfile.FileHeader;
import io.choerodon.agile.app.service.FilePathService;
import io.choerodon.agile.infra.enums.FileUploadBucket;
import io.choerodon.core.client.MessageClientC7n;
import org.apache.commons.compress.archivers.ArchiveEntry;
import org.apache.commons.compress.archivers.ArchiveInputStream;
import org.apache.commons.compress.archivers.tar.TarArchiveInputStream;
import org.apache.commons.compress.archivers.zip.ZipArchiveInputStream;
import org.apache.commons.compress.compressors.gzip.GzipCompressorInputStream;
import org.apache.commons.lang.StringUtils;
import org.hzero.boot.file.FileClient;
import org.hzero.boot.file.dto.FileDTO;
import org.modelmapper.ModelMapper;
import org.modelmapper.TypeToken;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Lazy;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.multipart.MultipartFile;

import java.io.BufferedInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;

import io.choerodon.agile.api.vo.StaticFileOperationHistorySocketVO;
import io.choerodon.agile.app.service.StaticFileCompressService;
import io.choerodon.agile.infra.dto.StaticFileCompressDTO;
import io.choerodon.agile.infra.dto.StaticFileHeaderDTO;
import io.choerodon.agile.infra.dto.StaticFileLineDTO;
import io.choerodon.agile.infra.dto.StaticFileOperationHistoryDTO;
import io.choerodon.agile.infra.mapper.*;
import io.choerodon.core.exception.CommonException;
import io.choerodon.core.oauth.DetailsHelper;
import io.choerodon.mybatis.helper.snowflake.SnowflakeHelper;

/**
 * @author chihao.ran@hand-china.com
 * 2021/01/11 11:28
 */
@Service
@Transactional(rollbackFor = Exception.class)
public class StaticFileCompressServiceImpl implements StaticFileCompressService {

    public static final Logger LOGGER = LoggerFactory.getLogger(StaticFileCompressServiceImpl.class);

    private static final String ZIP = ".ZIP";
    private static final String TAR = ".TAR";
    private static final String TAR_GZ = ".TAR.GZ";
    private static final String RAR = ".RAR";

    private static final String MACOSX = "__MACOSX";
    private static final String DS_STORE = ".DS_Store";
    private static final String INDEX_HTML = "index.html";
    private static final String FAILED = "failed";
    private static final String SUCCESS = "success";

    private static final String PATH_SPLIT = "/";

    private static final String IO_EXCEPTION_CODE = "error.uncompressed.io";
    private static final String INDEX_NULL_EXCEPTION_CODE = "error.staticFile.index.null";
    private static final String INDEX_EMPTY_EXCEPTION_CODE = "error.staticFile.index.empty";
    private static final String COMPRESSED_TYPE_EXCEPTION_CODE = "error.compressed.type.not.support";
    private static final String RAR4_EXCEPTION_CODE = "error.compressed.rar4";
    private static final String MALFORMED_EXCEPTION_CODE = "error.malformed.url";
    private static final String STATIC_FILE_WEBSOCKET_KEY = "agile-static-file";

    @Autowired
    private StaticFileHeaderMapper staticFileHeaderMapper;
    @Autowired
    private StaticFileLineMapper staticFileLineMapper;
    @Autowired
    private StaticFileIssueRelMapper staticFileIssueRelMapper;
    @Autowired
    private StaticFileOperationHistoryMapper staticFileOperationHistoryMapper;
    @Autowired
    private IssueMapper issueMapper;
    @Autowired
    private ModelMapper modelMapper;
    @Autowired
    private ObjectMapper objectMapper;
    @Autowired
    private MessageClientC7n messageClientC7n;
    @Autowired
    private FileClient fileClient;
    @Autowired
    @Lazy
    private SnowflakeHelper snowflakeHelper;
    @Autowired
    private FilePathService filePathService;

    @Override
    public void validFileType(List<MultipartFile> files) {
        files.forEach(multipartFile -> {
            boolean notIndex;
            //判断压缩类型，调用不同的解压方法
            String suffix = getSuffix(multipartFile.getOriginalFilename());
            String encode = getEncoding(multipartFile, suffix);
            switch (suffix) {
                case ZIP:
                case TAR:
                case TAR_GZ:
                    notIndex = validIndexExistByApache(multipartFile, suffix, encode);
                    break;
                case RAR:
                    notIndex = validIndexExistByRar(multipartFile);
                    break;
                default:
                    throw new CommonException(COMPRESSED_TYPE_EXCEPTION_CODE);
            }
            if (notIndex) {
                throw new CommonException(INDEX_NULL_EXCEPTION_CODE);
            }
        });
    }

    @Async
    @Override
    public void unCompress(List<StaticFileCompressDTO> staticFileCompressList, Long projectId, Long organizationId, List<StaticFileOperationHistoryDTO> staticFileCompressHistoryList, String issueId) {
        Long userId = DetailsHelper.getUserDetails().getUserId();
        LOGGER.info("start uncompress");
        sendProcess(staticFileCompressHistoryList, userId, projectId, issueId);
        staticFileCompressList.forEach(staticFileCompress -> {
            String suffix = getSuffix(staticFileCompress.getFileName());
            //判断压缩类型，调用不同的解压方法
            try {
                switch (suffix) {
                    case ZIP:
                    case TAR:
                    case TAR_GZ:
                        unCompressedByApache(staticFileCompress, projectId, organizationId, suffix, staticFileCompressHistoryList);
                        break;
                    case RAR:
                        unRar(staticFileCompress, projectId, organizationId, staticFileCompressHistoryList);
                        break;
                    default:
                        throw new CommonException(COMPRESSED_TYPE_EXCEPTION_CODE);
                }
            } catch (Exception e) {
                LOGGER.error(e.getMessage(), e);
                staticFileCompress.setStatus(FAILED);
                updateHistoryFailed(staticFileCompress.getStaticFileCompressHistory(), e.getMessage());
                sendProcess(staticFileCompressHistoryList, userId, projectId, issueId);
            }
        });
        LOGGER.info("end uncompress");
        //更新本次所有文件状态
        staticFileCompressList.forEach(staticFileCompress -> setFileStatus(staticFileCompress.getId(), staticFileCompress.getStatus()));
    }

    private void updateHistoryFailed(StaticFileOperationHistoryDTO staticFileCompressHistory, String errorMessage) {
        staticFileCompressHistory.setErrorMessage(errorMessage);
        this.updateHistoryStatus(staticFileCompressHistory, FAILED);
    }

    private void updateHistoryStatus(StaticFileOperationHistoryDTO staticFileCompressHistory, String status) {
        if (SUCCESS.equals(status)) {
            staticFileCompressHistory.setProcess(1.0);
        }
        staticFileCompressHistory.setStatus(status);
        staticFileOperationHistoryMapper.updateByPrimaryKeySelective(staticFileCompressHistory);
    }

    @Override
    public String getIndexPrefixPath(MultipartFile multipartFile, StaticFileCompressDTO staticFileCompressDTO) throws IOException {
        String result;
        String suffix = getSuffix(multipartFile.getOriginalFilename());
        //判断压缩类型，调用不同的解压方法
        switch (suffix) {
            case ZIP:
            case TAR:
            case TAR_GZ:
                staticFileCompressDTO.setEncode(getEncoding(multipartFile, suffix));
                result = getIndexPrefixPathByApache(multipartFile, suffix, staticFileCompressDTO);
                break;
            case RAR:
                result = getIndexPrefixPathByRar(multipartFile);
                break;
            default:
                throw new CommonException(COMPRESSED_TYPE_EXCEPTION_CODE);
        }
        return result;
    }

    private void unRar(StaticFileCompressDTO staticFileCompress, Long projectId, Long organizationId, List<StaticFileOperationHistoryDTO> staticFileCompressHistoryList) throws IOException {
        Long userId = DetailsHelper.getUserDetails().getUserId();
        List<StaticFileLineDTO> lineList = new ArrayList<>();
        List<String> urls = new ArrayList<>();
        String prefixPath = staticFileCompress.getPrefixPath();
        long nowSize = 0;
        double process = 0.0;
        int size = staticFileCompress.getSize();

        try {
            Archive archive = new Archive(staticFileCompress.getIn());
            FileHeader fileHeader;
            while (Objects.nonNull(fileHeader = archive.nextFileHeader())) {
                long newSize = fileHeader.getPackSize();
                nowSize += newSize;
                if (!fileHeader.isDirectory()) {
                    byte[] bytes = inputToByte(archive.getInputStream(fileHeader));
                    //跳过文件夹与不能读取数据的项
                    if (fileHeader.getFileName().contains(MACOSX) || fileHeader.getFileName().contains(DS_STORE) || newSize <= 0) {
                        //跳过冗余文件和空文件
                        continue;
                    }
                    String url = fileClient.uploadFile(organizationId, FileUploadBucket.AGILE_BUCKET.bucket(), null, getEntryFileName(fileHeader.getFileName()), bytes);
                    urls.add(url);
                    String relativePath = filePathService.generateRelativePath(url);
                    StaticFileLineDTO staticFileLine = new StaticFileLineDTO(
                            projectId,
                            organizationId,
                            staticFileCompress.getId(),
                            relativePath,
                            dealRelativePathSlash(fileHeader.getFileName(), prefixPath));
                    lineList.add(staticFileLine);
                    process = updateProcess(staticFileCompressHistoryList, staticFileCompress.getStaticFileCompressHistory(), size, nowSize, process, staticFileCompress.getIssueId());
                }
            }
        } catch (RarException e) {
            throw new CommonException(RAR4_EXCEPTION_CODE);
        }
        //获取上传的文件信息
        List<FileDTO> files = fileClient.getFiles(organizationId, FileUploadBucket.AGILE_BUCKET.bucket(), urls);
        Map<String, FileDTO> fileMap = files.stream().collect(Collectors.toMap(file -> filePathService.generateRelativePath(file.getFileUrl()), file -> file));
        lineList.forEach(line -> {
            //设置行的文件类型及其记录其他信息
            line.setId(snowflakeHelper.next());
            line.setCreatedBy(userId);
            line.setLastUpdatedBy(userId);
            line.setFileType(fileMap.get(line.getUrl()) != null ? fileMap.get(line.getUrl()).getFileType() : null);
        });
        staticFileLineMapper.batchInsert(lineList);
        updateHistoryStatus(staticFileCompress.getStaticFileCompressHistory(), SUCCESS);
        staticFileCompress.setStatus(SUCCESS);
        sendProcess(staticFileCompressHistoryList, staticFileCompress.getStaticFileCompressHistory().getUserId(), projectId, staticFileCompress.getIssueId());
    }

    /**
     * 可使用apache解压工具进行解压的流程一致，根据文件后缀名获取不同的压缩流
     *
     * @param staticFileCompress            解压参数
     * @param projectId                     项目id
     * @param organizationId                组织id
     * @param suffix                        文件后缀名
     * @param staticFileCompressHistoryList 解压操作历史记录
     * @throws IOException io错误
     */
    private void unCompressedByApache(StaticFileCompressDTO staticFileCompress, Long projectId, Long organizationId, String suffix, List<StaticFileOperationHistoryDTO> staticFileCompressHistoryList) throws IOException {
        Long userId = DetailsHelper.getUserDetails().getUserId();
        StaticFileHeaderDTO update = new StaticFileHeaderDTO();
        update.setId(staticFileCompress.getId());
        int size = staticFileCompress.getSize();
        double process = 0.0;
        List<StaticFileLineDTO> lineList = new ArrayList<>();
        List<String> urls = new ArrayList<>();

        String prefixPath = staticFileCompress.getPrefixPath();
        try (
                BufferedInputStream bufferedInputStream = new BufferedInputStream(staticFileCompress.getIn());
                ArchiveInputStream in = getArchiveInputStream(bufferedInputStream, suffix, staticFileCompress.getEncode())
        ) {
            ArchiveEntry entry;
            while (Objects.nonNull(entry = in.getNextEntry())) {
                int availableSize = bufferedInputStream.available();
                if (!entry.isDirectory() && in.canReadEntryData(entry)) {
                    byte[] bytes = inputToByte(in);
                    int newSize = bytes.length;
                    //跳过文件夹与不能读取数据的项
                    if (entry.getName().contains(MACOSX) || entry.getName().contains(DS_STORE) || newSize <= 0) {
                        //跳过冗余文件
                        continue;
                    }
                    //文件上传
                    String url = fileClient.uploadFile(organizationId, FileUploadBucket.AGILE_BUCKET.bucket(), null, getEntryFileName(entry.getName()), bytes);
                    urls.add(url);
                    String relativePath = filePathService.generateRelativePath(url);
                    StaticFileLineDTO staticFileLine = new StaticFileLineDTO(
                            projectId,
                            organizationId,
                            staticFileCompress.getId(),
                            relativePath,
                            dealRelativePath(entry.getName(), prefixPath));
                    lineList.add(staticFileLine);
                }
                process = updateProcess(staticFileCompressHistoryList, staticFileCompress.getStaticFileCompressHistory(), size, (size - availableSize), process, staticFileCompress.getIssueId());
            }
            //获取上传的文件信息
            List<FileDTO> files = fileClient.getFiles(organizationId, FileUploadBucket.AGILE_BUCKET.bucket(), urls);
            Map<String, FileDTO> fileMap = files.stream().collect(Collectors.toMap(file -> filePathService.generateRelativePath(file.getFileUrl()), file -> file));
            lineList.forEach(line -> {
                //设置行的文件类型及其记录其他信息
                line.setId(snowflakeHelper.next());
                line.setCreatedBy(userId);
                line.setLastUpdatedBy(userId);
                line.setFileType(fileMap.get(line.getUrl()) != null ? fileMap.get(line.getUrl()).getFileType() : null);
            });
            staticFileLineMapper.batchInsert(lineList);
            updateHistoryStatus(staticFileCompress.getStaticFileCompressHistory(), SUCCESS);
            staticFileCompress.setStatus(SUCCESS);
            sendProcess(staticFileCompressHistoryList, staticFileCompress.getStaticFileCompressHistory().getUserId(), projectId, staticFileCompress.getIssueId());
        }
    }

    private double updateProcess(List<StaticFileOperationHistoryDTO> staticFileCompressHistoryList, StaticFileOperationHistoryDTO staticFileCompressHistory, int toTalSize, long nowSize, double process, String issueId) {
        double nowProcess = new BigDecimal(nowSize).divide(new BigDecimal(toTalSize), 2, RoundingMode.HALF_UP).doubleValue();
        if (nowProcess - process < 0.1) {
            return process;
        }
        staticFileCompressHistory.setProcess(nowProcess);
        sendProcess(staticFileCompressHistoryList, staticFileCompressHistory.getUserId(), staticFileCompressHistory.getProjectId(), issueId);
        return nowProcess;
    }

    private void setFileStatus(Long id, String status) {
        staticFileHeaderMapper.updateFileStatus(id, status);
    }

    private ArchiveInputStream getArchiveInputStream(BufferedInputStream bufferedInputStream, String suffix, String encode) throws IOException {
        //根据文件后缀名获取不同的压缩流
        switch (suffix) {
            case ZIP:
                return new ZipArchiveInputStream(bufferedInputStream, encode);
            case TAR:
                return new TarArchiveInputStream(bufferedInputStream, encode);
            case TAR_GZ:
                return new TarArchiveInputStream(new GzipCompressorInputStream(bufferedInputStream), encode);
            default:
                throw new CommonException(COMPRESSED_TYPE_EXCEPTION_CODE);
        }
    }

    private String dealRelativePath(String name, String prefixPath) {
        if (StringUtils.isBlank(prefixPath)) {
            return name;
        }
        int dot = name.indexOf(prefixPath);
        if ((dot > -1) && (dot < (name.length()))) {
            return name.substring(name.indexOf(prefixPath) + prefixPath.length());
        }
        return name;
    }

    private String dealRelativePathSlash(String name, String prefixPath) {
        return dealRelativePath(name.replace("\\", PATH_SPLIT), prefixPath);
    }

    public String getIndexPrefixPathByApache(MultipartFile multipartFile, String suffix, StaticFileCompressDTO staticFileCompressDTO) {
        String result = "";
        boolean notIndex = true;
        try (BufferedInputStream bufferedInputStream = new BufferedInputStream(multipartFile.getInputStream());
             ArchiveInputStream in = getArchiveInputStream(bufferedInputStream, suffix, staticFileCompressDTO.getEncode())
        ) {
            ArchiveEntry entry;
            while (Objects.nonNull(entry = in.getNextEntry())) {
                String entryName = entry.getName();
                // 忽略mac压缩包冗余文件夹
                if (entryName.contains(MACOSX) || entryName.contains(DS_STORE)) {
                    continue;
                }
                if (!entry.isDirectory() && entryName.endsWith(INDEX_HTML)) {
                    notIndex = false;
                    result = getPrefixPath(entry.getName());
                }
            }
        } catch (IOException e) {
            throw new CommonException(IO_EXCEPTION_CODE, e);
        }
        if (notIndex) {
            throw new CommonException(INDEX_NULL_EXCEPTION_CODE);
        }
        return result;
    }

    private String getIndexPrefixPathByRar(MultipartFile multipartFile) throws IOException {
        String result = "";
        boolean notIndex = true;
        try {
            Archive archive = new Archive(multipartFile.getInputStream());
            FileHeader fileHeader;
            while (Objects.nonNull(fileHeader = archive.nextFileHeader())) {
                String name = fileHeader.getFileName();
                if (name.contains(MACOSX) || name.contains(DS_STORE)) {
                    continue;
                }
                if (!fileHeader.isDirectory() && name.endsWith(INDEX_HTML)) {
                    notIndex = false;
                    result = getPrefixPathSlash(name);
                }
            }
        } catch (RarException e) {
            throw new CommonException(RAR4_EXCEPTION_CODE);
        }
        if (notIndex) {
            throw new CommonException(INDEX_NULL_EXCEPTION_CODE);
        }
        return result;
    }

    private String getPrefixPath(String name) {
        return name.substring(0, name.lastIndexOf(PATH_SPLIT) + 1);
    }

    private String getPrefixPathSlash(String name) {
        return getPrefixPath(name.replace("\\", PATH_SPLIT));
    }

    private String getUpperName(String fileName) {
        if ((fileName != null) && (fileName.length() > 0)) {
            return fileName.toUpperCase();
        }
        return "";
    }

//    private String dealUrl(String url) {
//        String dealUrl;
//        try {
//            URL netUrl = new URL(url);
//            dealUrl = netUrl.getFile().substring(FileUploadBucket.AGILE_BUCKET.bucket().length() + 2);
//        } catch (MalformedURLException e) {
//            throw new CommonException(MALFORMED_EXCEPTION_CODE, e);
//        }
//        return dealUrl;
//    }

    private byte[] inputToByte(InputStream inStream) throws IOException {
        ByteArrayOutputStream swapStream = new ByteArrayOutputStream();
        byte[] buff = new byte[1024];
        int rc;
        while ((rc = inStream.read(buff, 0, 1024)) > 0) {
            swapStream.write(buff, 0, rc);
        }
        return swapStream.toByteArray();
    }

    private String getEntryFileName(String entryName) {
        return entryName.substring(entryName.lastIndexOf(PATH_SPLIT) + 1);
    }

    private boolean validIndexExistByApache(MultipartFile multipartFile, String suffix, String encode) {
        boolean notIndex = true;
        try (BufferedInputStream bufferedInputStream = new BufferedInputStream(multipartFile.getInputStream());
             ArchiveInputStream in = getArchiveInputStream(bufferedInputStream, suffix, encode)
        ) {
            ArchiveEntry entry;
            while (Objects.nonNull(entry = in.getNextEntry())) {
                String entryName = entry.getName();
                // 忽略mac压缩包冗余文件夹
                if (entryName.contains(MACOSX) || entryName.contains(DS_STORE)) {
                    continue;
                }
                if (!entry.isDirectory() && entryName.endsWith(INDEX_HTML)) {
                    if (inputToByte(in).length <= 0) {
                        throw new CommonException(INDEX_EMPTY_EXCEPTION_CODE);
                    }
                    notIndex = false;
                }
            }
        } catch (IOException e) {
            throw new CommonException(IO_EXCEPTION_CODE, e);
        }
        return notIndex;
    }

    private boolean validIndexExistByRar(MultipartFile multipartFile) {
        boolean notIndex = true;
        try {
            Archive archive = new Archive(multipartFile.getInputStream());
            FileHeader fileHeader;
            while (Objects.nonNull(fileHeader = archive.nextFileHeader())) {
                String name = fileHeader.getFileName();
                if (name.contains(MACOSX) || name.contains(DS_STORE)) {
                    continue;
                }
                if (!fileHeader.isDirectory() && name.endsWith(INDEX_HTML)) {
                    if (inputToByte(archive.getInputStream(fileHeader)).length <= 0) {
                        throw new CommonException(INDEX_EMPTY_EXCEPTION_CODE);
                    }
                    notIndex = false;
                }
            }
        } catch (RarException | IOException e) {
            throw new CommonException(RAR4_EXCEPTION_CODE);
        }
        return notIndex;
    }

    private void sendProcess(List<StaticFileOperationHistoryDTO> staticFileOperationHistoryList, Long userId, Long projectId, String issueId) {
        String messageCode = STATIC_FILE_WEBSOCKET_KEY + "-" + projectId + "-" + issueId;
        List<StaticFileOperationHistorySocketVO> staticFileOperationHistorySocketList = modelMapper.map(staticFileOperationHistoryList, new TypeToken<List<StaticFileOperationHistorySocketVO>>() {
        }.getType());
        String message = null;
        try {
            message = objectMapper.writeValueAsString(staticFileOperationHistorySocketList);
        } catch (JsonProcessingException e) {
            LOGGER.error("object to json error", e);
        }
        messageClientC7n.sendByUserId(userId, messageCode, message);
    }

    private String getSuffix(String fileName) {
        String upperFileName = getUpperName(fileName);
        if (upperFileName.endsWith(TAR_GZ)) {
            return TAR_GZ;
        }
        return upperFileName.substring(upperFileName.lastIndexOf('.'));
    }

    @Override
    public String getEncoding(MultipartFile multipartFile, String suffix){
        String resultCode = "GBK";
        try (BufferedInputStream bufferedInputStream = new BufferedInputStream(multipartFile.getInputStream());
             ArchiveInputStream in = getArchiveInputStream(bufferedInputStream, suffix, resultCode)
        ) {
            ArchiveEntry entry;
            while (Objects.nonNull(entry = in.getNextEntry())) {
                if(isMessyCode((entry.getName()))){
                   resultCode = "UTF-8";
                   break;
                }
            }
        } catch (IOException e) {
            resultCode = "UTF-8";
        }
        return resultCode;
    }

    private boolean isMessyCode(String str) {
        boolean isUtf8 = true;
        byte[] buffer;
        try {
            buffer = str.getBytes("GBK");
        }catch (Exception e){
            return false;
        }
        int end = buffer.length;
        int i = 0;
        while (i < end) {
            boolean isBreak = true;
            byte temp = buffer[i];
            if ((temp & 0x80) == 0) {
                isBreak = false;
            } else if ((temp & 0xC0) == 0xC0 && (temp & 0x20) == 0) {
                if (i + 1 < end && (buffer[i + 1] & 0x80) == 0x80 && (buffer[i + 1] & 0x40) == 0) {
                    i = i + 1;
                    isBreak = false;
                }
            } else if ((temp & 0xE0) == 0xE0 && (temp & 0x10) == 0) {
                if (i + 2 < end && (buffer[i + 1] & 0x80) == 0x80 && (buffer[i + 1] & 0x40) == 0
                        && (buffer[i + 2] & 0x80) == 0x80 && (buffer[i + 2] & 0x40) == 0) {
                    i = i + 2;
                    isBreak = false;
                }
            } else if ((temp & 0xF0) == 0xF0 && (temp & 0x08) == 0) {
                if (i + 3 < end && (buffer[i + 1] & 0x80) == 0x80 && (buffer[i + 1] & 0x40) == 0
                        && (buffer[i + 2] & 0x80) == 0x80 && (buffer[i + 2] & 0x40) == 0
                        && (buffer[i + 3] & 0x80) == 0x80 && (buffer[i + 3] & 0x40) == 0) {
                    i = i + 3;
                    isBreak = false;
                }
            }
            if (isBreak) {
                isUtf8 = false;
                break;
            }
            i = i + 1;
        }
        return isUtf8;
    }
}
