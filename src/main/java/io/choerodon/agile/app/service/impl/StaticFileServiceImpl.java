package io.choerodon.agile.app.service.impl;

import com.github.junrar.Archive;
import com.github.junrar.exception.RarException;
import com.github.junrar.rarfile.FileHeader;
import org.apache.commons.compress.archivers.ArchiveEntry;
import org.apache.commons.compress.archivers.ArchiveInputStream;
import org.apache.commons.compress.archivers.tar.TarArchiveInputStream;
import org.apache.commons.compress.archivers.zip.ZipArchiveInputStream;
import org.apache.commons.compress.compressors.gzip.GzipCompressorInputStream;
import org.apache.commons.compress.utils.IOUtils;
import org.apache.commons.lang.StringUtils;
import org.hzero.boot.file.FileClient;
import org.hzero.boot.file.dto.FileDTO;
import org.modelmapper.ModelMapper;
import org.modelmapper.TypeToken;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;
import org.springframework.util.ObjectUtils;
import org.springframework.web.context.request.WebRequest;
import org.springframework.web.multipart.MultipartFile;
import org.springframework.web.multipart.MultipartHttpServletRequest;

import java.io.*;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLDecoder;
import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import io.choerodon.agile.api.vo.StaticFileHeaderVO;
import io.choerodon.agile.api.vo.StaticFileRelatedVO;
import io.choerodon.agile.app.service.StaticFileDealService;
import io.choerodon.agile.app.service.StaticFileService;
import io.choerodon.agile.infra.dto.StaticFileHeaderDTO;
import io.choerodon.agile.infra.dto.StaticFileIssueRelDTO;
import io.choerodon.agile.infra.dto.StaticFileLineDTO;
import io.choerodon.agile.infra.dto.business.IssueDTO;
import io.choerodon.agile.infra.mapper.IssueMapper;
import io.choerodon.agile.infra.mapper.StaticFileHeaderMapper;
import io.choerodon.agile.infra.mapper.StaticFileIssueRelMapper;
import io.choerodon.agile.infra.mapper.StaticFileLineMapper;
import io.choerodon.agile.infra.utils.ProjectUtil;
import io.choerodon.core.exception.CommonException;
import io.choerodon.core.oauth.DetailsHelper;
import io.choerodon.mybatis.helper.snowflake.SnowflakeHelper;

/**
 * @author chihao.ran@hand-china.com
 * 2021/01/11 11:28
 */
@Service
@Transactional(rollbackFor = Exception.class)
public class StaticFileServiceImpl implements StaticFileService {

    private static final String ZIP = ".ZIP";
    private static final String TAR = ".TAR";
    private static final String TAR_GZ = ".TAR.GZ";
    private static final String RAR = ".RAR";

    private static final String MACOSX = "__MACOSX";
    private static final String DS_STORE = ".DS_Store";
    private static final String INDEX_HTML = "index.html";
    private static final String INDEX = "index";
    private static final String PATH_SPLIT = "/";

    private static final String BUCKET_NAME = "agile-service";

    private static final String IO_EXCEPTION_CODE = "error.uncompressed.io";
    private static final String INDEX_NULL_EXCEPTION_CODE = "error.staticFile.index.null";
    private static final String INDEX_EMPTY_EXCEPTION_CODE = "error.staticFile.index.empty";
    private static final String COMPRESSED_TYPE_EXCEPTION_CODE = "error.compressed.type.not.support";
    private static final String RAR4_EXCEPTION_CODE = "error.compressed.rar4";
    private static final String DELETE_NULL_EXCEPTION_CODE = "error.delete.staticFileHeader.null";
    private static final String HEADER_NULL_EXCEPTION_CODE = "error.staticFileHeader.null";
    private static final String HEADER_ID_NULL_EXCEPTION_CODE = "error.fileHeaderId.null";
    private static final String ISSUE_NULL_EXCEPTION_CODE = "error.issue.null";
    private static final String ISSUE_ID_NULL_EXCEPTION_CODE = "error.issueId.null";
    private static final String MALFORMED_EXCEPTION_CODE = "error.malformed.url";
    private static final String RELATED_EXCEPTION_CODE = "error.file.issue.related.exist";

    @Value("${services.attachment.url}")
    private String attachmentUrl;
    @Autowired
    private StaticFileDealService staticFileDealService;
    @Autowired
    private StaticFileHeaderMapper staticFileHeaderMapper;
    @Autowired
    private StaticFileLineMapper staticFileLineMapper;
    @Autowired
    private StaticFileIssueRelMapper staticFileIssueRelMapper;
    @Autowired
    private IssueMapper issueMapper;
    @Autowired
    private ProjectUtil projectUtil;
    @Autowired
    private FileClient fileClient;
    @Autowired
    private ModelMapper modelMapper;
    @Autowired
    private SnowflakeHelper snowflakeHelper;

    @Override
    public List<StaticFileHeaderVO> uploadStaticFile(Long projectId, Long issueId, HttpServletRequest request) {
        List<StaticFileHeaderVO> result = new ArrayList<>();
        Long organizationId = projectUtil.getOrganizationId(projectId);
        List<MultipartFile> files = ((MultipartHttpServletRequest) request).getFiles("file");
        if (!CollectionUtils.isEmpty(files)) {
            validFileType(files);
            for (MultipartFile multipartFile : files) {
                try {
                    StaticFileHeaderVO staticFileHeaderVO = unCompress(multipartFile, projectId, issueId, organizationId);
                    if (!ObjectUtils.isEmpty(staticFileHeaderVO)) {
                        result.add(staticFileHeaderVO);
                    }
                } catch (IOException e) {
                    throw new CommonException(IO_EXCEPTION_CODE, e);
                }
            }
        }
        return result;
    }

    private void validFileType(List<MultipartFile> files) {
        files.forEach(multipartFile -> {
            boolean notIndex;
            String fileUpperName = getUpperName(multipartFile.getOriginalFilename());
            //判断压缩类型，调用不同的解压方法
            if (fileUpperName.endsWith(ZIP)) {
                notIndex = validIndexExistByApache(multipartFile, ZIP);
            } else if (fileUpperName.endsWith(TAR)) {
                notIndex = validIndexExistByApache(multipartFile, TAR);
            } else if (fileUpperName.endsWith(TAR_GZ)) {
                notIndex = validIndexExistByApache(multipartFile, TAR_GZ);
            } else if (fileUpperName.endsWith(RAR)) {
                notIndex = validIndexExistByRar(multipartFile);
            } else {
                throw new CommonException(COMPRESSED_TYPE_EXCEPTION_CODE);
            }
            if (notIndex) {
                throw new CommonException(INDEX_NULL_EXCEPTION_CODE);
            }
        });
    }

    @Override
    public ResponseEntity<byte[]> selectStaticFileResult(Long fileHeaderId, WebRequest webRequest, HttpServletRequest httpRequest, HttpServletResponse httpResponse) throws IOException {
        String path;
        try {
            path = URLDecoder.decode(httpRequest.getServletPath(), Charset.defaultCharset().name());
        } catch (UnsupportedEncodingException e) {
            path = httpRequest.getServletPath();
        }
        // 获取文件相对路径
        String fileHeaderIdStr = String.valueOf(fileHeaderId);
        String relativePath = path.substring(path.indexOf(fileHeaderIdStr) + fileHeaderIdStr.length() + 1);
        if (INDEX.equals(relativePath)) {
            relativePath = INDEX_HTML;
        }
        StaticFileLineDTO record = new StaticFileLineDTO();
        record.setHeaderId(fileHeaderId);
        record.setRelativePath(relativePath);
        StaticFileLineDTO file = staticFileLineMapper.selectOne(record);
        if (ObjectUtils.isEmpty(file)) {
            return ResponseEntity.ok().body(new byte[0]);
        }
        return ResponseEntity.ok()
                .contentType(MediaType.parseMediaType(file.getFileType()))
                .body(getFileByteArray(file));
    }

    @Override
    public List<StaticFileHeaderVO> selectFileListByProject(Long projectId) {
        StaticFileHeaderDTO record = new StaticFileHeaderDTO();
        record.setProjectId(projectId);
        List<StaticFileHeaderDTO> staticFileHeaderList = staticFileHeaderMapper.select(record);
        List<StaticFileHeaderVO> result = modelMapper.map(staticFileHeaderList, new TypeToken<List<StaticFileHeaderVO>>() {
        }.getType());
        if (!CollectionUtils.isEmpty(result)) {
            result.forEach(staticFileHeaderVO ->
                    staticFileHeaderVO.setUrl(getRealUrl(staticFileHeaderVO.getUrl()))
            );
        }
        return result;
    }

    @Override
    public List<StaticFileHeaderVO> selectFileListByIssue(Long projectId, Long issueId) {
        List<StaticFileHeaderDTO> staticFileHeaderList = staticFileHeaderMapper.selectFileListByIssue(projectId, issueId);
        List<StaticFileHeaderVO> result = modelMapper.map(staticFileHeaderList, new TypeToken<List<StaticFileHeaderVO>>() {
        }.getType());
        if (!CollectionUtils.isEmpty(result)) {
            result.forEach(staticFileHeaderVO ->
                    staticFileHeaderVO.setUrl(getRealUrl(staticFileHeaderVO.getUrl()))
            );
        }
        return result;
    }

    @Override
    public void deleteStaticFileRelated(Long projectId, Long fileHeaderId, Long issueId) {
        StaticFileHeaderDTO staticFileHeader = staticFileHeaderMapper.selectByPrimaryKey(fileHeaderId);
        if (ObjectUtils.isEmpty(staticFileHeader)) {
            throw new CommonException(DELETE_NULL_EXCEPTION_CODE);
        }
        StaticFileIssueRelDTO record = new StaticFileIssueRelDTO();
        record.setProjectId(projectId);
        record.setStaticFileId(fileHeaderId);
        record.setIssueId(issueId);
        staticFileIssueRelMapper.delete(record);
    }

    @Override
    public void deleteStaticFile(Long projectId, Long fileHeaderId) {
        List<String> fileUrls = new ArrayList<>();
        Long organizationId = projectUtil.getOrganizationId(projectId);
        StaticFileHeaderDTO staticFileHeader = staticFileHeaderMapper.selectByPrimaryKey(fileHeaderId);
        if (ObjectUtils.isEmpty(staticFileHeader)) {
            throw new CommonException(DELETE_NULL_EXCEPTION_CODE);
        }
        StaticFileLineDTO lineRecord = new StaticFileLineDTO();
        lineRecord.setHeaderId(fileHeaderId);
        List<StaticFileLineDTO> fileLineList = staticFileLineMapper.select(lineRecord);
        fileUrls.add(getRealUrl(staticFileHeader.getUrl()));
        if (!CollectionUtils.isEmpty(fileLineList)) {
            fileLineList.forEach(staticFileLineDTO ->
                    fileUrls.add(getRealUrl(staticFileLineDTO.getUrl()))
            );
        }
        if (!CollectionUtils.isEmpty(fileUrls)) {
            fileClient.deleteFileByUrl(organizationId, BUCKET_NAME, fileUrls);
        }

        StaticFileIssueRelDTO relRecord = new StaticFileIssueRelDTO();
        relRecord.setProjectId(projectId);
        relRecord.setStaticFileId(fileHeaderId);

        staticFileIssueRelMapper.delete(relRecord);
        staticFileLineMapper.delete(lineRecord);
        staticFileDealService.deleteBase(fileHeaderId);
    }

    @Override
    public List<StaticFileHeaderVO> updateStaticFileRelatedIssue(Long projectId, StaticFileRelatedVO staticFileRelatedVO) {
        if (ObjectUtils.isEmpty(staticFileRelatedVO.getIssueId())) {
            throw new CommonException(ISSUE_ID_NULL_EXCEPTION_CODE);
        }
        if (CollectionUtils.isEmpty(staticFileRelatedVO.getFileHeaderIds())) {
            throw new CommonException(HEADER_ID_NULL_EXCEPTION_CODE);
        }

        IssueDTO issueRecord = new IssueDTO();
        issueRecord.setIssueId(staticFileRelatedVO.getIssueId());
        issueRecord.setProjectId(projectId);
        IssueDTO issue = issueMapper.selectOne(issueRecord);
        if (ObjectUtils.isEmpty(issue)) {
            throw new CommonException(ISSUE_NULL_EXCEPTION_CODE);
        }
        String ids = staticFileRelatedVO.getFileHeaderIds().stream().map(String::valueOf).collect(Collectors.joining(","));
        List<StaticFileHeaderDTO> staticFileHeaders = staticFileHeaderMapper.selectByIds(ids);
        if (CollectionUtils.isEmpty(staticFileHeaders) || staticFileHeaders.size() < staticFileRelatedVO.getFileHeaderIds().size()) {
            throw new CommonException(HEADER_NULL_EXCEPTION_CODE);
        }
        List<StaticFileHeaderVO> staticFileHeaderVOList = new ArrayList<>();
        staticFileHeaders.forEach(staticFileHeaderDTO -> {
            StaticFileIssueRelDTO newRelDTO = new StaticFileIssueRelDTO();
            newRelDTO.setIssueId(staticFileRelatedVO.getIssueId());
            newRelDTO.setStaticFileId(staticFileHeaderDTO.getId());
            StaticFileIssueRelDTO oldRelDTO = staticFileIssueRelMapper.selectOne(newRelDTO);
            if (!ObjectUtils.isEmpty(oldRelDTO)) {
                //关联关系已存在
                throw new CommonException(RELATED_EXCEPTION_CODE);
            }
            //创建关联关系
            newRelDTO.setProjectId(projectId);
            newRelDTO.setOrganizationId(projectUtil.getOrganizationId(projectId));
            staticFileIssueRelMapper.insert(newRelDTO);

            StaticFileHeaderVO staticFileHeaderVO = modelMapper.map(staticFileHeaderDTO, StaticFileHeaderVO.class);
            staticFileHeaderVO.setUrl(getRealUrl(staticFileHeaderVO.getUrl()));
            staticFileHeaderVOList.add(staticFileHeaderVO);
        });
        return staticFileHeaderVOList;
    }

    @Override
    public List<StaticFileHeaderVO> selectFileListExcludeIssue(Long projectId, Long issueId) {
        List<StaticFileHeaderVO> staticFileHeaderVOList = new ArrayList<>();
        List<StaticFileHeaderDTO> staticFileHeaderList = staticFileHeaderMapper.selectFileListExcludeIssue(projectId, issueId);
        staticFileHeaderList.forEach(staticFileHeaderDTO -> {
            StaticFileHeaderVO staticFileHeaderVO = modelMapper.map(staticFileHeaderDTO, StaticFileHeaderVO.class);
            staticFileHeaderVO.setUrl(getRealUrl(staticFileHeaderVO.getUrl()));
            staticFileHeaderVOList.add(staticFileHeaderVO);
        });
        return staticFileHeaderVOList;
    }

    private byte[] getFileByteArray(StaticFileLineDTO file) throws IOException {
        InputStream inputStream =
                fileClient.downloadFile(file.getOrganizationId(), BUCKET_NAME, getRealUrl(file.getUrl()));
        return IOUtils.toByteArray(inputStream);
    }

    private StaticFileHeaderVO unCompress(MultipartFile multipartFile, Long projectId, Long issueId, Long organizationId) throws IOException {
        String fileUpperName = getUpperName(multipartFile.getOriginalFilename());
        //判断压缩类型，调用不同的解压方法
        if (fileUpperName.endsWith(ZIP)) {
            return unCompressedByApache(multipartFile, projectId, issueId, organizationId, ZIP);
        } else if (fileUpperName.endsWith(TAR)) {
            return unCompressedByApache(multipartFile, projectId, issueId, organizationId, TAR);
        } else if (fileUpperName.endsWith(TAR_GZ)) {
            return unCompressedByApache(multipartFile, projectId, issueId, organizationId, TAR_GZ);
        } else if (fileUpperName.endsWith(RAR)) {
            return unRar(multipartFile, projectId, issueId, organizationId);
        } else {
            throw new CommonException(COMPRESSED_TYPE_EXCEPTION_CODE);
        }
    }

    private StaticFileHeaderVO unRar(MultipartFile multipartFile, Long projectId, Long issueId, Long organizationId) throws IOException {
        Long userId = DetailsHelper.getUserDetails().getUserId();
        String headerUrl = fileClient.uploadFile(organizationId, BUCKET_NAME, null, multipartFile.getOriginalFilename(), multipartFile);
        StaticFileHeaderDTO staticFileHeader = createStaticFileHeader(projectId, organizationId, issueId, dealUrl(headerUrl), multipartFile.getOriginalFilename());

        staticFileDealService.createBase(staticFileHeader);

        List<StaticFileLineDTO> lineList = new ArrayList<>();
        List<String> urls = new ArrayList<>();
        String prefixPath = getIndexPrefixPathByRar(multipartFile);

        try {
            Archive archive = new Archive(multipartFile.getInputStream());
            FileHeader fileHeader;
            while (Objects.nonNull(fileHeader = archive.nextFileHeader())) {
                if (!fileHeader.isDirectory()) {
                    byte[] bytes = inputToByte(archive.getInputStream(fileHeader));
                    //跳过文件夹与不能读取数据的项
                    if (fileHeader.getFileName().contains(MACOSX) || fileHeader.getFileName().contains(DS_STORE) || bytes.length <= 0) {
                        //跳过冗余文件和空文件
                        continue;
                    }
                    String url = fileClient.uploadFile(organizationId, BUCKET_NAME, null, getEntryFileName(fileHeader.getFileName()), bytes);
                    urls.add(url);
                    StaticFileLineDTO staticFileLine = new StaticFileLineDTO(
                            projectId,
                            organizationId,
                            staticFileHeader.getId(),
                            dealUrl(url),
                            dealRelativePathSlash(fileHeader.getFileName(), prefixPath));
                    lineList.add(staticFileLine);
                }
            }
        } catch (RarException e) {
            throw new CommonException(RAR4_EXCEPTION_CODE);
        }
        //获取上传的文件信息
        List<FileDTO> files = fileClient.getFiles(organizationId, BUCKET_NAME, urls);
        Map<String, FileDTO> fileMap = files.stream().collect(Collectors.toMap(file -> dealUrl(file.getFileUrl()), file -> file));
        lineList.forEach(line -> {
            //设置行的文件类型及其记录其他信息
            line.setId(snowflakeHelper.next());
            line.setCreatedBy(userId);
            line.setLastUpdatedBy(userId);
            line.setFileType(fileMap.get(line.getUrl()) != null ? fileMap.get(line.getUrl()).getFileType() : null);
        });
        staticFileLineMapper.batchInsert(lineList);
        StaticFileHeaderVO staticFileHeaderVO = modelMapper.map(staticFileHeader, StaticFileHeaderVO.class);
        staticFileHeaderVO.setUrl(getRealUrl(staticFileHeaderVO.getUrl()));
        return staticFileHeaderVO;
    }

    /**
     * 可使用apache解压工具进行解压的流程一致，根据文件后缀名获取不同的压缩流
     *
     * @param multipartFile  上传的文件
     * @param projectId      项目id
     * @param issueId        问题id
     * @param organizationId 组织id
     * @param suffix         文件后缀名
     * @return 创建的静态文件头记录
     * @throws IOException io错误
     */
    private StaticFileHeaderVO unCompressedByApache(MultipartFile multipartFile, Long projectId, Long issueId, Long organizationId, String suffix) throws IOException {
        Long userId = DetailsHelper.getUserDetails().getUserId();
        String headerUrl = fileClient.uploadFile(organizationId, BUCKET_NAME, null, multipartFile.getOriginalFilename(), multipartFile);
        StaticFileHeaderDTO staticFileHeader = createStaticFileHeader(projectId, organizationId, issueId, dealUrl(headerUrl), multipartFile.getOriginalFilename());

        List<StaticFileLineDTO> lineList = new ArrayList<>();
        List<String> urls = new ArrayList<>();

        String prefixPath = getIndexPrefixPathByApache(multipartFile, suffix);
        try (
                BufferedInputStream bufferedInputStream = new BufferedInputStream(multipartFile.getInputStream());
                //根据文件后缀名获取不同的压缩流
                ArchiveInputStream in = getArchiveInputStream(bufferedInputStream, suffix)
        ) {
            ArchiveEntry entry;
            while (Objects.nonNull(entry = in.getNextEntry())) {
                if (!entry.isDirectory() && in.canReadEntryData(entry)) {
                    //跳过文件夹与不能读取数据的项
                    byte[] bytes = inputToByte(in);
                    if (entry.getName().contains(MACOSX) || entry.getName().contains(DS_STORE) || bytes.length <= 0) {
                        //跳过冗余文件
                        continue;
                    }
                    //文件上传
                    String url = fileClient.uploadFile(organizationId, BUCKET_NAME, null, getEntryFileName(entry.getName()), bytes);
                    urls.add(url);
                    StaticFileLineDTO staticFileLine = new StaticFileLineDTO(
                            projectId,
                            organizationId,
                            staticFileHeader.getId(),
                            dealUrl(url),
                            dealRelativePath(entry.getName(), prefixPath));
                    lineList.add(staticFileLine);
                }
            }
            //获取上传的文件信息
            List<FileDTO> files = fileClient.getFiles(organizationId, BUCKET_NAME, urls);
            Map<String, FileDTO> fileMap = files.stream().collect(Collectors.toMap(file -> dealUrl(file.getFileUrl()), file -> file));
            lineList.forEach(line -> {
                //设置行的文件类型及其记录其他信息
                line.setId(snowflakeHelper.next());
                line.setCreatedBy(userId);
                line.setLastUpdatedBy(userId);
                line.setFileType(fileMap.get(line.getUrl()) != null ? fileMap.get(line.getUrl()).getFileType() : null);
            });
            staticFileLineMapper.batchInsert(lineList);
        }
        StaticFileHeaderVO staticFileHeaderVO = modelMapper.map(staticFileHeader, StaticFileHeaderVO.class);
        staticFileHeaderVO.setUrl(getRealUrl(staticFileHeaderVO.getUrl()));
        return staticFileHeaderVO;
    }

    private StaticFileHeaderDTO createStaticFileHeader(Long projectId, Long organizationId, Long issueId, String url, String originalFilename) {
        StaticFileHeaderDTO staticFileHeader = new StaticFileHeaderDTO(
                projectId,
                organizationId,
                url,
                originalFilename);
        staticFileDealService.createBase(staticFileHeader);
        StaticFileIssueRelDTO staticFileIssueRelDTO = new StaticFileIssueRelDTO(
                staticFileHeader.getId(),
                issueId,
                projectId,
                organizationId
        );
        staticFileIssueRelMapper.insert(staticFileIssueRelDTO);
        return staticFileHeader;
    }

    private ArchiveInputStream getArchiveInputStream(BufferedInputStream bufferedInputStream, String suffix) throws IOException {
        if (TAR.equals(suffix)) {
            return new TarArchiveInputStream(bufferedInputStream);
        } else if (ZIP.equals(suffix)) {
            return new ZipArchiveInputStream(bufferedInputStream);
        } else if (TAR_GZ.equals(suffix)) {
            return new TarArchiveInputStream(new GzipCompressorInputStream(bufferedInputStream));
        } else {
            throw new CommonException(COMPRESSED_TYPE_EXCEPTION_CODE);
        }
    }

    private String dealRelativePath(String name, String prefixPath) {
        if (StringUtils.isBlank(prefixPath)) {
            return name;
        }
        int dot = name.lastIndexOf(prefixPath);
        if ((dot > -1) && (dot < (name.length()))) {
            return name.substring(name.lastIndexOf(prefixPath) + prefixPath.length());
        }
        return name;
    }

    private String dealRelativePathSlash(String name, String prefixPath) {
        return dealRelativePath(name.replace("\\", PATH_SPLIT), prefixPath);
    }

    private String getIndexPrefixPathByApache(MultipartFile multipartFile, String suffix) {
        String result = "";
        boolean notIndex = true;
        try (BufferedInputStream bufferedInputStream = new BufferedInputStream(multipartFile.getInputStream());
             ArchiveInputStream in = getArchiveInputStream(bufferedInputStream, suffix)
        ) {
            ArchiveEntry entry;
            while (Objects.nonNull(entry = in.getNextEntry())) {
                String entryName = entry.getName();
                // 忽略mac压缩包冗余文件夹
                if (entryName.contains(MACOSX) || entryName.contains(DS_STORE)) {
                    continue;
                }
                if (!entry.isDirectory() && entryName.contains(INDEX_HTML)) {
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
                if (!fileHeader.isDirectory() && name.contains(INDEX_HTML)) {
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

    private String dealUrl(String url) {
        String dealUrl;
        try {
            URL netUrl = new URL(url);
            dealUrl = netUrl.getFile().substring(BUCKET_NAME.length() + 2);
        } catch (MalformedURLException e) {
            throw new CommonException(MALFORMED_EXCEPTION_CODE, e);
        }
        return dealUrl;
    }

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

    private String getRealUrl(String url) {
        return attachmentUrl + "/" + BUCKET_NAME + "/" + url;
    }


    private boolean validIndexExistByApache(MultipartFile multipartFile, String suffix) {
        boolean notIndex = true;
        try (BufferedInputStream bufferedInputStream = new BufferedInputStream(multipartFile.getInputStream());
             ArchiveInputStream in = getArchiveInputStream(bufferedInputStream, suffix)
        ) {
            ArchiveEntry entry;
            while (Objects.nonNull(entry = in.getNextEntry())) {
                String entryName = entry.getName();
                // 忽略mac压缩包冗余文件夹
                if (entryName.contains(MACOSX) || entryName.contains(DS_STORE)) {
                    continue;
                }
                if (!entry.isDirectory() && entryName.contains(INDEX_HTML)) {
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
                if (!fileHeader.isDirectory() && name.contains(INDEX_HTML)) {
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
}
