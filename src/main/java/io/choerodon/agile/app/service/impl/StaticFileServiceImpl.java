package io.choerodon.agile.app.service.impl;

import io.choerodon.agile.app.service.FilePathService;
import io.choerodon.agile.infra.enums.FileUploadBucket;
import org.apache.commons.compress.utils.IOUtils;
import org.hzero.boot.file.FileClient;
import org.modelmapper.ModelMapper;
import org.modelmapper.TypeToken;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.CacheControl;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;
import org.springframework.util.ObjectUtils;
import org.springframework.web.context.request.WebRequest;
import org.springframework.web.multipart.MultipartFile;
import org.springframework.web.multipart.MultipartHttpServletRequest;

import java.io.IOException;
import java.io.InputStream;
import java.io.UnsupportedEncodingException;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLDecoder;
import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import io.choerodon.agile.api.vo.StaticFileHeaderVO;
import io.choerodon.agile.api.vo.StaticFileRelatedVO;
import io.choerodon.agile.app.service.StaticFileCompressService;
import io.choerodon.agile.app.service.StaticFileDealService;
import io.choerodon.agile.app.service.StaticFileService;
import io.choerodon.agile.infra.dto.*;
import io.choerodon.agile.infra.dto.business.IssueDTO;
import io.choerodon.agile.infra.mapper.*;
import io.choerodon.agile.infra.utils.EncryptionUtils;
import io.choerodon.agile.infra.utils.ProjectUtil;
import io.choerodon.core.exception.CommonException;
import io.choerodon.core.oauth.DetailsHelper;

/**
 * @author chihao.ran@hand-china.com
 * 2021/01/11 11:28
 */
@Service
@Transactional(rollbackFor = Exception.class)
public class StaticFileServiceImpl implements StaticFileService {


    private static final String DOING = "doing";
    private static final String DECOMPRESS = "decompress";
    private static final String DELETE = "delete";
    private static final String INDEX_HTML = "index.html";
    private static final String START_HTML = "start_with_pages.html";
    private static final String HTML_CONTENT_TYPE = "text/html";
    private static final String SVG_CONTENT_TYPE = "image/svg+xml";
    private static final String INDEX = "index";

    private static final String IO_EXCEPTION_CODE = "error.uncompressed.io";
    private static final String DELETE_NULL_EXCEPTION_CODE = "error.delete.staticFileHeader.null";
    private static final String HEADER_NULL_EXCEPTION_CODE = "error.staticFileHeader.null";
    private static final String HEADER_ID_NULL_EXCEPTION_CODE = "error.fileHeaderId.null";
    private static final String ISSUE_NULL_EXCEPTION_CODE = "error.issue.null";
    private static final String ISSUE_ID_NULL_EXCEPTION_CODE = "error.issueId.null";
    private static final String MALFORMED_EXCEPTION_CODE = "error.malformed.url";
    private static final String RELATED_EXCEPTION_CODE = "error.file.issue.related.exist";

//    @Value("${services.attachment.url}")
//    private String attachmentUrl;
    @Autowired
    private StaticFileDealService staticFileDealService;
    @Autowired
    private StaticFileCompressService staticFileCompressService;
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
    private ProjectUtil projectUtil;
    @Autowired
    private FileClient fileClient;
    @Autowired
    private ModelMapper modelMapper;
    @Autowired
    private FilePathService filePathService;

    @Override
    public List<StaticFileHeaderVO> uploadStaticFile(Long projectId, Long issueId, HttpServletRequest request) {
        String issueIdStr = EncryptionUtils.encrypt(issueId);
        List<StaticFileOperationHistoryDTO> staticFileCompressHistoryList = new ArrayList<>();
        List<StaticFileCompressDTO> staticFileCompress = new ArrayList<>();
        List<StaticFileHeaderVO> result = new ArrayList<>();
        Long organizationId = projectUtil.getOrganizationId(projectId);
        List<MultipartFile> files = ((MultipartHttpServletRequest) request).getFiles("file");
        if (!CollectionUtils.isEmpty(files)) {
            staticFileCompressService.validFileType(files);
            for (MultipartFile multipartFile : files) {
                String headerUrl = fileClient.uploadFile(organizationId, FileUploadBucket.AGILE_BUCKET.bucket(), null, multipartFile.getOriginalFilename(), multipartFile);
                String relativePath = filePathService.generateRelativePath(headerUrl);
                StaticFileHeaderDTO staticFileHeader = createStaticFileHeader(projectId, organizationId, issueId, relativePath, multipartFile.getOriginalFilename());
                issueMapper.updateIssueLastUpdateInfo(issueId, projectId, DetailsHelper.getUserDetails().getUserId());
                if (!ObjectUtils.isEmpty(staticFileHeader)) {
                    StaticFileHeaderVO staticFileHeaderVO = modelMapper.map(staticFileHeader, StaticFileHeaderVO.class);
                    staticFileHeaderVO.setUrl(filePathService.generateFullPath(FileUploadBucket.AGILE_BUCKET.bucket(), staticFileHeaderVO.getUrl()));
                    result.add(staticFileHeaderVO);

                    StaticFileOperationHistoryDTO staticFileCompressHistory = createStaticFileCompressHistory(staticFileHeader);
                    staticFileCompressHistoryList.add(staticFileCompressHistory);

                    staticFileCompress.add(getStaticFileCompressParams(staticFileHeader, multipartFile, staticFileCompressHistory, issueIdStr));
                }
            }
            staticFileCompressService.unCompress(staticFileCompress, projectId, organizationId, staticFileCompressHistoryList, issueIdStr);
        }
        return result;
    }

    private StaticFileOperationHistoryDTO createStaticFileDeleteHistory(StaticFileHeaderDTO staticFileHeader) {
        StaticFileOperationHistoryDTO staticFileCompressHistory = new StaticFileOperationHistoryDTO(
                staticFileHeader.getProjectId(),
                staticFileHeader.getOrganizationId(),
                staticFileHeader.getId(),
                staticFileHeader.getCreatedBy(),
                DELETE,
                DOING);
        if (staticFileOperationHistoryMapper.insertSelective(staticFileCompressHistory) != 1) {
            throw new CommonException("error.staticFileOperationHistoryDTO.insert");
        }
        StaticFileOperationHistoryDTO res = staticFileOperationHistoryMapper.selectByPrimaryKey(staticFileCompressHistory.getId());
        res.setFileName(staticFileHeader.getFileName());
        return res;
    }

    private StaticFileOperationHistoryDTO createStaticFileCompressHistory(StaticFileHeaderDTO staticFileHeader) {
        StaticFileOperationHistoryDTO staticFileCompressHistory = new StaticFileOperationHistoryDTO(
                staticFileHeader.getProjectId(),
                staticFileHeader.getOrganizationId(),
                staticFileHeader.getId(),
                staticFileHeader.getCreatedBy(),
                DECOMPRESS,
                DOING);
        if (staticFileOperationHistoryMapper.insertSelective(staticFileCompressHistory) != 1) {
            throw new CommonException("error.staticFileOperationHistoryDTO.insert");
        }
        StaticFileOperationHistoryDTO res = staticFileOperationHistoryMapper.selectByPrimaryKey(staticFileCompressHistory.getId());
        res.setFileName(staticFileHeader.getFileName());
        res.setProcess(0.0);
        return res;
    }

    private StaticFileCompressDTO getStaticFileCompressParams(StaticFileHeaderDTO staticFileHeader, MultipartFile multipartFile, StaticFileOperationHistoryDTO staticFileCompressHistory, String issueId) {
        StaticFileCompressDTO staticFileCompressDTO = new StaticFileCompressDTO();
        staticFileCompressDTO.setId(staticFileHeader.getId());
        staticFileCompressDTO.setFileName(multipartFile.getOriginalFilename());
        staticFileCompressDTO.setStaticFileCompressHistory(staticFileCompressHistory);
        staticFileCompressDTO.setIssueId(issueId);
        try {
            staticFileCompressDTO.setIn(multipartFile.getInputStream());
            staticFileCompressDTO.setSize(multipartFile.getBytes().length);
            staticFileCompressDTO.setPrefixPath(staticFileCompressService.getIndexPrefixPath(multipartFile, staticFileCompressDTO));
        } catch (IOException e) {
            throw new CommonException(IO_EXCEPTION_CODE);
        }
        return staticFileCompressDTO;
    }

    @Override
    public ResponseEntity<byte[]> selectStaticFileResult(Long fileHeaderId, WebRequest webRequest, HttpServletRequest httpRequest, HttpServletResponse httpResponse) throws IOException {
        String fileHeaderIdStr = fileHeaderId.toString();
        StaticFileLineDTO file;

        String path;
        try {
            path = URLDecoder.decode(httpRequest.getServletPath(), Charset.defaultCharset().name());
        } catch (UnsupportedEncodingException e) {
            path = httpRequest.getServletPath();
        }
        // 获取文件相对路径
        String relativePath = path.substring(path.indexOf(fileHeaderIdStr) + fileHeaderIdStr.length() + 1);
        if (INDEX.equals(relativePath)) {
            file = getIndexFile(fileHeaderId);
        } else {
            StaticFileLineDTO record = new StaticFileLineDTO();
            record.setHeaderId(fileHeaderId);
            record.setRelativePath(relativePath);
            file = staticFileLineMapper.selectOne(record);
        }
        if (ObjectUtils.isEmpty(file)) {
            return ResponseEntity.ok().body(new byte[0]);
        }
        return ResponseEntity.ok()
                .contentType(MediaType.parseMediaType(dealContentType(file)))
                .cacheControl(getCacheControl(file))
                .body(getFileByteArray(file));
    }

    private StaticFileHeaderDTO createStaticFileHeader(Long projectId, Long organizationId, Long issueId, String url, String originalFilename) {
        StaticFileHeaderDTO staticFileHeader = new StaticFileHeaderDTO(
                projectId,
                organizationId,
                url,
                originalFilename,
                DOING);
        StaticFileIssueRelDTO staticFileIssueRelDTO = new StaticFileIssueRelDTO(
                staticFileHeader.getId(),
                issueId,
                projectId,
                organizationId
        );
        return staticFileDealService.createBase(staticFileHeader, staticFileIssueRelDTO);
    }

    private CacheControl getCacheControl(StaticFileLineDTO file) {
        if (HTML_CONTENT_TYPE.equals(file.getFileType())) {
            return CacheControl.noStore();
        } else {
            return CacheControl.maxAge(1, TimeUnit.DAYS).cachePublic();
        }
    }

    private StaticFileLineDTO getIndexFile(Long fileHeaderId) {
        StaticFileLineDTO file;
        StaticFileLineDTO record = new StaticFileLineDTO();
        record.setHeaderId(fileHeaderId);

        record.setRelativePath(START_HTML);
        file = staticFileLineMapper.selectOne(record);
        if(ObjectUtils.isEmpty(file)){
            record.setRelativePath(INDEX_HTML);
            file = staticFileLineMapper.selectOne(record);
        }
        return file;
    }

    private String dealContentType(StaticFileLineDTO file) {
        if (file.getRelativePath().toUpperCase().endsWith(".SVG")) {
            return SVG_CONTENT_TYPE;
        } else {
            return file.getFileType();
        }
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
                    staticFileHeaderVO.setUrl(filePathService.generateFullPath(FileUploadBucket.AGILE_BUCKET.bucket(), staticFileHeaderVO.getUrl()))
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
                    staticFileHeaderVO.setUrl(filePathService.generateFullPath(FileUploadBucket.AGILE_BUCKET.bucket(), staticFileHeaderVO.getUrl()))
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
        record.setIssueId(issueId);
        staticFileDealService.deleteStaticFileRelated(staticFileHeader, record);
        issueMapper.updateIssueLastUpdateInfo(issueId, projectId, DetailsHelper.getUserDetails().getUserId());
    }

    @Override
    public void deleteStaticFile(Long projectId, Long fileHeaderId) {
        List<String> fileUrls = new ArrayList<>();

        StaticFileHeaderDTO headerRecord = new StaticFileHeaderDTO();
        headerRecord.setId(fileHeaderId);
        headerRecord.setProjectId(projectId);
        StaticFileHeaderDTO staticFileHeader = staticFileHeaderMapper.selectOne(headerRecord);
        if (ObjectUtils.isEmpty(staticFileHeader)) {
            throw new CommonException(DELETE_NULL_EXCEPTION_CODE);
        }

        StaticFileLineDTO lineRecord = new StaticFileLineDTO();
        lineRecord.setHeaderId(fileHeaderId);
        List<StaticFileLineDTO> fileLineList = staticFileLineMapper.select(lineRecord);
        fileUrls.add(filePathService.generateFullPath(FileUploadBucket.AGILE_BUCKET.bucket(), staticFileHeader.getUrl()));
        if (!CollectionUtils.isEmpty(fileLineList)) {
            fileLineList.forEach(staticFileLineDTO ->
                    fileUrls.add(filePathService.generateFullPath(FileUploadBucket.AGILE_BUCKET.bucket(), staticFileLineDTO.getUrl()))
            );
        }

        StaticFileOperationHistoryDTO staticFileDeleteHistory = createStaticFileDeleteHistory(staticFileHeader);

        StaticFileIssueRelDTO relRecord = new StaticFileIssueRelDTO();
        relRecord.setProjectId(projectId);
        staticFileHeaderMapper.updateFileStatus(staticFileHeader.getId(), DOING);
        List<Long> issueIds = staticFileHeaderMapper.selectRelatedIssueId(staticFileHeader.getId(), projectId);
        if (CollectionUtils.isEmpty(issueIds)) {
            issueMapper.batchUpdateIssueLastUpdateInfo(issueIds, projectId, DetailsHelper.getUserDetails().getUserId());
        }
        staticFileDealService.deleteBase(relRecord, staticFileHeader, fileUrls, staticFileDeleteHistory);
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
            staticFileDealService.updateStaticFileRelatedIssue(newRelDTO, staticFileHeaderDTO);

            StaticFileHeaderVO staticFileHeaderVO = modelMapper.map(staticFileHeaderDTO, StaticFileHeaderVO.class);
            staticFileHeaderVO.setUrl(filePathService.generateFullPath(FileUploadBucket.AGILE_BUCKET.bucket(), staticFileHeaderVO.getUrl()));
            staticFileHeaderVOList.add(staticFileHeaderVO);
        });
        issueMapper.updateIssueLastUpdateInfo(staticFileRelatedVO.getIssueId(), projectId, DetailsHelper.getUserDetails().getUserId());
        return staticFileHeaderVOList;
    }

    @Override
    public List<StaticFileHeaderVO> selectFileListExcludeIssue(Long projectId, Long issueId) {
        List<StaticFileHeaderVO> staticFileHeaderVOList = new ArrayList<>();
        List<StaticFileHeaderDTO> staticFileHeaderList = staticFileHeaderMapper.selectFileListExcludeIssue(projectId, issueId);
        staticFileHeaderList.forEach(staticFileHeaderDTO -> {
            StaticFileHeaderVO staticFileHeaderVO = modelMapper.map(staticFileHeaderDTO, StaticFileHeaderVO.class);
            staticFileHeaderVO.setUrl(filePathService.generateFullPath(FileUploadBucket.AGILE_BUCKET.bucket(), staticFileHeaderVO.getUrl()));
            staticFileHeaderVOList.add(staticFileHeaderVO);
        });
        return staticFileHeaderVOList;
    }

    @Override
    public StaticFileHeaderVO selectFileHeaderById(Long fileHeaderId) {
        StaticFileHeaderDTO staticFileHeaderDTO = staticFileHeaderMapper.selectByPrimaryKey(fileHeaderId);
        StaticFileHeaderVO staticFileHeaderVO = modelMapper.map(staticFileHeaderDTO, StaticFileHeaderVO.class);
        staticFileHeaderVO.setUrl(filePathService.generateFullPath(FileUploadBucket.AGILE_BUCKET.bucket(), staticFileHeaderVO.getUrl()));
        return staticFileHeaderVO;
    }

    private byte[] getFileByteArray(StaticFileLineDTO file) throws IOException {
        InputStream inputStream =
                fileClient.downloadFile(file.getOrganizationId(), FileUploadBucket.AGILE_BUCKET.bucket(), filePathService.generateFullPath(FileUploadBucket.AGILE_BUCKET.bucket(), file.getUrl()));
        return IOUtils.toByteArray(inputStream);
    }

//    private String getRealUrl(String url) {
//        return attachmentUrl + "/" + FileUploadBucket.AGILE_BUCKET.bucket() + "/" + url;
//    }

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

}
