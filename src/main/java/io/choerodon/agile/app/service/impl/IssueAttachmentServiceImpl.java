package io.choerodon.agile.app.service.impl;

import io.choerodon.agile.api.vo.IssueAttachmentCombineVO;
import io.choerodon.agile.app.service.FilePathService;
import io.choerodon.agile.app.service.IIssueAttachmentService;
import io.choerodon.agile.infra.dto.IssueAttachmentDTO;
import io.choerodon.agile.infra.dto.TestCaseAttachmentDTO;
import io.choerodon.agile.infra.dto.business.IssueDTO;
import io.choerodon.agile.infra.enums.FileUploadBucket;
import io.choerodon.agile.infra.feign.CustomFileRemoteService;
import io.choerodon.agile.infra.mapper.IssueMapper;
import io.choerodon.agile.infra.utils.BaseFieldUtil;
import io.choerodon.agile.infra.utils.ProjectUtil;
import io.choerodon.core.exception.CommonException;
import io.choerodon.agile.api.vo.IssueAttachmentVO;
import io.choerodon.agile.app.service.IssueAttachmentService;
import io.choerodon.agile.infra.mapper.IssueAttachmentMapper;
import io.choerodon.core.oauth.DetailsHelper;

import org.hzero.boot.file.FileClient;
import org.hzero.core.util.ResponseUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;
import org.springframework.util.ObjectUtils;
import org.springframework.web.multipart.MultipartFile;
import org.springframework.web.multipart.MultipartHttpServletRequest;

import javax.servlet.http.HttpServletRequest;
import java.net.URLDecoder;
import java.util.*;

/**
 * Created by HuangFuqiang@choerodon.io on 2018/5/16.
 * Email: fuqianghuang01@gmail.com
 */
@Service
@Transactional(rollbackFor = Exception.class)
public class IssueAttachmentServiceImpl implements IssueAttachmentService {

    private static final Logger LOGGER = LoggerFactory.getLogger(IssueAttachmentServiceImpl.class);


    @Autowired
    private IssueAttachmentMapper issueAttachmentMapper;
    @Autowired
    private IssueMapper issueMapper;
    @Autowired
    private IIssueAttachmentService iIssueAttachmentService;

//    @Value("${services.attachment.url}")
//    private String attachmentUrl;

    @Autowired
    private FileClient fileClient;

    @Autowired
    private CustomFileRemoteService customFileRemoteService;

    @Autowired
    private ProjectUtil projectUtil;
    @Autowired
    private FilePathService filePathService;

    @Override
    public IssueAttachmentDTO dealIssue(Long projectId, Long issueId, String fileName, String url) {
        IssueAttachmentDTO issueAttachmentDTO = new IssueAttachmentDTO();
        issueAttachmentDTO.setProjectId(projectId);
        issueAttachmentDTO.setIssueId(issueId);
        issueAttachmentDTO.setFileName(fileName);
        issueAttachmentDTO.setUrl(url);
        issueAttachmentDTO.setCommentId(1L);
        IssueAttachmentDTO result = iIssueAttachmentService.createBase(issueAttachmentDTO);
        issueMapper.updateIssueLastUpdateInfo(issueAttachmentDTO.getIssueId(), issueAttachmentDTO.getProjectId(), DetailsHelper.getUserDetails().getUserId());
        return result;
    }

    @Override
    public List<TestCaseAttachmentDTO> migrateIssueAttachment() {
        List<TestCaseAttachmentDTO> list = issueAttachmentMapper.listAttachmentDTO();
        if (CollectionUtils.isEmpty(list)) {
            return new ArrayList<>();
        }
        return list;
    }

    @Override
    public void validCombineUpload(IssueAttachmentCombineVO issueAttachmentCombineVO) {
        if (issueAttachmentCombineVO.getIssueId() == null) {
            throw new CommonException("error.attachmentRule.issueId");
        }
        if (issueAttachmentCombineVO.getFileName() == null) {
            throw new CommonException("error.attachmentRule.fileName");
        }
        if (issueAttachmentCombineVO.getGuid() == null) {
            throw new CommonException("error.attachmentRule.guId");
        }
        IssueDTO issueDTO = issueMapper.selectByPrimaryKey(issueAttachmentCombineVO.getIssueId());
        if (ObjectUtils.isEmpty(issueDTO)) {
            throw new CommonException("error.attachmentRule.issue");
        }
    }

    @Override
    public IssueAttachmentVO attachmentCombineUpload(Long projectId, IssueAttachmentCombineVO issueAttachmentCombineVO) {
        Long organizationId = projectUtil.getOrganizationId(projectId);
        Map<String, String> args = new HashMap<>(1);
        args.put("bucketName", FileUploadBucket.AGILE_BUCKET.bucket());
        String url = ResponseUtils.getResponse(customFileRemoteService.fragmentCombineBlock(
                organizationId,
                issueAttachmentCombineVO.getGuid(),
                issueAttachmentCombineVO.getFileName(),
                args),
                String.class,
                (httpStatus, response) -> {
                }, exceptionResponse -> {
                    LOGGER.error("combine fragment failed: {}", exceptionResponse.getMessage());
                    throw new CommonException(exceptionResponse.getMessage());
                });
        if (url == null) {
            throw new CommonException("error.attachment.combine.failed");
        }
        String relativePath = filePathService.generateRelativePath(url);
        IssueAttachmentDTO result = dealIssue(projectId, issueAttachmentCombineVO.getIssueId(),
                issueAttachmentCombineVO.getFileName(), relativePath);
        IssueAttachmentVO issueAttachmentVO = new IssueAttachmentVO();
        BeanUtils.copyProperties(result, issueAttachmentVO);
        String fullPath = filePathService.generateFullPath(result.getUrl());
        issueAttachmentVO.setUrl(fullPath);
        return issueAttachmentVO;
    }

//    private String dealUrl(String url) {
//        String dealUrl = null;
//        try {
//            URL netUrl = new URL(url);
//            dealUrl = netUrl.getFile().substring(FileUploadBucket.AGILE_BUCKET.bucket().length() + 2);
//        } catch (MalformedURLException e) {
//            throw new CommonException("error.malformed.url", e);
//        }
//        return dealUrl;
//    }

    @Override
    public List<IssueAttachmentVO> create(Long projectId, Long issueId, HttpServletRequest request) {
        List<MultipartFile> files = ((MultipartHttpServletRequest) request).getFiles("file");
        if (!CollectionUtils.isEmpty(files)) {
            for (MultipartFile multipartFile : files) {
                String fileName = multipartFile.getOriginalFilename();
                Long organizationId = projectUtil.getOrganizationId(projectId);
                String url = fileClient.uploadFile(organizationId, FileUploadBucket.AGILE_BUCKET.bucket(), null, fileName, multipartFile);
                String relativePath = filePathService.generateRelativePath(url);
                dealIssue(projectId, issueId, fileName, relativePath);
            }
        }
        IssueAttachmentDTO issueAttachmentDTO = new IssueAttachmentDTO();
        issueAttachmentDTO.setIssueId(issueId);
        List<IssueAttachmentDTO> issueAttachmentDTOList = issueAttachmentMapper.select(issueAttachmentDTO);
        List<IssueAttachmentVO> result = new ArrayList<>();
        if (issueAttachmentDTOList != null && !issueAttachmentDTOList.isEmpty()) {
            issueAttachmentDTOList.forEach(attachment -> {
                IssueAttachmentVO issueAttachmentVO = new IssueAttachmentVO();
                BeanUtils.copyProperties(attachment, issueAttachmentVO);
                String fullPath = filePathService.generateFullPath(attachment.getUrl());
                issueAttachmentVO.setUrl(fullPath);
                result.add(issueAttachmentVO);
            });
        }
        return result;
    }

    @Override
    public Boolean delete(Long projectId, Long issueAttachmentId) {
        IssueAttachmentDTO issueAttachmentDTO = issueAttachmentMapper.selectByPrimaryKey(issueAttachmentId);
        if (issueAttachmentDTO == null) {
            throw new CommonException("error.attachment.get");
        }
        if (!issueAttachmentDTO.getProjectId().equals(projectId)) {
            throw new CommonException("error.project.id.does.not.correspond");
        }
        Boolean result = iIssueAttachmentService.deleteBase(issueAttachmentDTO.getAttachmentId());
        BaseFieldUtil.updateIssueLastUpdateInfo(issueAttachmentDTO.getIssueId(), issueAttachmentDTO.getProjectId());
        String url = null;
        // 判断是否有其他关联
        if (existAttachmentIssueRel(projectId, issueAttachmentDTO)) {
            return result;
        }
        try {
            url = URLDecoder.decode(issueAttachmentDTO.getUrl(), "UTF-8");
            String fullPath = filePathService.generateFullPath(url);
            Long organizationId = projectUtil.getOrganizationId(projectId);
            ResponseUtils.getResponse(customFileRemoteService.deleteFileByUrl(organizationId, FileUploadBucket.AGILE_BUCKET.bucket(), Arrays.asList(fullPath)), String.class);
        } catch (Exception e) {
            LOGGER.error("error.attachment.delete", e);
        }
        return result;
    }

    private boolean existAttachmentIssueRel(Long projectId, IssueAttachmentDTO issueAttachmentDTO) {
        IssueAttachmentDTO select = new IssueAttachmentDTO();
        select.setProjectId(projectId);
        select.setUrl(issueAttachmentDTO.getUrl());
        select.setFileName(issueAttachmentDTO.getFileName());
        return issueAttachmentMapper.selectCount(select) > 0;
    }

    @Override
    public List<String> uploadForAddress(Long projectId, HttpServletRequest request) {
        List<MultipartFile> files = ((MultipartHttpServletRequest) request).getFiles("file");
        if (CollectionUtils.isEmpty(files)) {
            throw new CommonException("error.attachment.exits");
        }
        List<String> result = new ArrayList<>();
        for (MultipartFile multipartFile : files) {
            String fileName = multipartFile.getOriginalFilename();
            Long organizationId = projectUtil.getOrganizationId(projectId);
            String url = fileClient.uploadFile(organizationId, FileUploadBucket.AGILE_BUCKET.bucket(), null, fileName, multipartFile);
            String relativePath = filePathService.generateRelativePath(url);
            result.add(filePathService.generateFullPath(relativePath));
        }
        return result;
    }

    @Override
    public int deleteByIssueId(Long issueId) {
        IssueAttachmentDTO issueAttachmentDTO = new IssueAttachmentDTO();
        issueAttachmentDTO.setIssueId(issueId);
        return issueAttachmentMapper.delete(issueAttachmentDTO);
    }

    @Override
    public void copyIssueAttachments(Long projectId, Long issueId, Long newIssueId) {
        IssueAttachmentDTO searchDTO = new IssueAttachmentDTO();
        searchDTO.setIssueId(issueId);
        List<IssueAttachmentDTO> issueAttachmentDTOList = issueAttachmentMapper.select(searchDTO);
        if (ObjectUtils.isEmpty(issueAttachmentDTOList)) {
            return;
        }
        searchDTO.setIssueId(newIssueId);
        List<IssueAttachmentDTO> existList = issueAttachmentMapper.select(searchDTO);
        if (!ObjectUtils.isEmpty(existList)) {
            return;
        }
        issueAttachmentDTOList.forEach(v -> {
            IssueAttachmentDTO create = new IssueAttachmentDTO();
            create.setIssueId(newIssueId);
            create.setCommentId(1L);
            create.setUrl(v.getUrl());
            create.setFileName(v.getFileName());
            create.setProjectId(projectId);
            iIssueAttachmentService.createBase(create);
        });
        issueMapper.updateIssueLastUpdateInfo(newIssueId, projectId, DetailsHelper.getUserDetails().getUserId());
    }
}
