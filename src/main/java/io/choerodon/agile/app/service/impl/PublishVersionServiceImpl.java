package io.choerodon.agile.app.service.impl;

import io.choerodon.agile.infra.dto.PublishVersionTreeClosureDTO;
import io.choerodon.agile.infra.mapper.PublishVersionTreeClosureMapper;
import io.choerodon.agile.infra.utils.AssertUtilsForCommonException;
import org.apache.commons.lang3.StringUtils;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.ObjectUtils;
import org.springframework.web.multipart.MultipartFile;
import org.xml.sax.SAXException;

import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;
import javax.xml.parsers.ParserConfigurationException;

import io.choerodon.agile.api.vo.*;
import io.choerodon.agile.app.service.PublishVersionService;
import io.choerodon.agile.app.service.PomService;
import io.choerodon.agile.infra.dto.PublishVersionDTO;
import io.choerodon.agile.infra.enums.ProjectCategory;
import io.choerodon.agile.infra.feign.BaseFeignClient;
import io.choerodon.agile.infra.feign.operator.DevopsClientOperator;
import io.choerodon.agile.infra.feign.vo.ProjectCategoryDTO;
import io.choerodon.agile.infra.mapper.PublishVersionIssueRelMapper;
import io.choerodon.agile.infra.mapper.PublishVersionMapper;
import io.choerodon.agile.infra.mapper.ProductAppVersionRelMapper;
import io.choerodon.agile.infra.utils.ConvertUtil;
import io.choerodon.core.domain.Page;
import io.choerodon.core.exception.CommonException;
import io.choerodon.mybatis.pagehelper.PageHelper;
import io.choerodon.mybatis.pagehelper.domain.PageRequest;

/**
 * @author superlee
 * @since 2021-03-10
 */
@Service
@Transactional(rollbackFor = Exception.class)
public class PublishVersionServiceImpl implements PublishVersionService {

    @Autowired
    private ModelMapper modelMapper;
    @Autowired
    private PublishVersionMapper publishVersionMapper;
    @Autowired
    private PublishVersionIssueRelMapper publishVersionIssueRelMapper;
    @Autowired
    private ProductAppVersionRelMapper productAppVersionRelMapper;
    @Autowired
    private BaseFeignClient baseFeignClient;
    @Autowired
    private DevopsClientOperator devopsClientOperator;
    @Autowired
    private PomService pomService;
    @Autowired
    private PublishVersionTreeClosureMapper publishVersionTreeClosureMapper;

    private static final String GROUP_ID_EMPTY_EXCEPTION = "error.publish.version.groupId.empty";
    private static final String VERSION_ALIAS_EMPTY_EXCEPTION = "error.publish.version.alias.empty";
    private static final String VERSION_EMPTY_EXCEPTION = "error.publish.version.version.empty";
    private static final String ARTIFACT_ID_EMPTY_EXCEPTION = "error.publish.version.artifactId.empty";
    private static final String SERVICE_CODE_EMPTY_EXCEPTION = "error.publish.version.serviceCode.empty";
    private static final String PUBLISH_VERSION_NOT_EXIST_EXCEPTION = "error.publish.version.not.existed";

    @Override
    public PublishVersionVO create(Long projectId, PublishVersionVO publishVersionVO) {
        String version = publishVersionVO.getVersion();
        String groupId = publishVersionVO.getGroupId();
        String artifactId = publishVersionVO.getArtifactId();
        String serviceCode = publishVersionVO.getServiceCode();
        createValidator(publishVersionVO, version, groupId, artifactId, serviceCode);
        publishVersionVO.setProjectId(projectId);
        publishVersionVO.setOrganizationId(ConvertUtil.getOrganizationId(projectId));
        if (!StringUtils.isEmpty(version)
                && !StringUtils.isEmpty(groupId)
                && !StringUtils.isEmpty(artifactId)
                && !StringUtils.isEmpty(serviceCode)
                && isExisted(projectId, publishVersionVO)) {
            throw new CommonException("error.publish.version.duplicate");
        }
        PublishVersionDTO dto = modelMapper.map(publishVersionVO, PublishVersionDTO.class);
        if (publishVersionMapper.insertSelective(dto) != 1) {
            throw new CommonException("error.publish.version.insert");
        }
        createPublishVersionTreeClosure(dto.getId(), projectId, dto.getOrganizationId());
        return modelMapper.map(publishVersionMapper.selectByPrimaryKey(dto.getId()), PublishVersionVO.class);
    }

    @Override
    public Boolean isExisted(Long projectId, PublishVersionVO publishVersionVO) {
        String groupId = publishVersionVO.getGroupId();
        String artifactId = publishVersionVO.getArtifactId();
        String version = publishVersionVO.getVersion();
        String serviceCode = publishVersionVO.getServiceCode();
        AssertUtilsForCommonException.notNull(projectId, "error.publish.version.projectId.null");
        AssertUtilsForCommonException.notEmpty(groupId, GROUP_ID_EMPTY_EXCEPTION);
        AssertUtilsForCommonException.notEmpty(artifactId, ARTIFACT_ID_EMPTY_EXCEPTION);
        AssertUtilsForCommonException.notEmpty(version, VERSION_EMPTY_EXCEPTION);
        AssertUtilsForCommonException.notEmpty(serviceCode, SERVICE_CODE_EMPTY_EXCEPTION);

        PublishVersionDTO dto = new PublishVersionDTO();
        dto.setProjectId(projectId);
        dto.setGroupId(groupId);
        dto.setArtifactId(artifactId);
        dto.setVersion(version);
        dto.setServiceCode(serviceCode);
        List<PublishVersionDTO> list = publishVersionMapper.select(dto);
        if (publishVersionVO.getId() == null) {
            return !list.isEmpty();
        } else {
            Long id = list.get(0).getId();
            return !id.equals(publishVersionVO.getId());
        }
    }

    @Override
    public PublishVersionVO update(Long projectId, Long publishVersionId, PublishVersionVO publishVersionVO) {
        PublishVersionDTO publishVersionDTO = publishVersionMapper.selectByPrimaryKey(publishVersionId);
        if (ObjectUtils.isEmpty(publishVersionDTO)) {
            throw new CommonException(PUBLISH_VERSION_NOT_EXIST_EXCEPTION);
        }
        String groupId = publishVersionVO.getGroupId();
        String version = publishVersionVO.getVersion();
        String artifactId = publishVersionVO.getArtifactId();
        String serviceCode = publishVersionVO.getServiceCode();
        if (!StringUtils.isEmpty(version)
                && !StringUtils.isEmpty(groupId)
                && !StringUtils.isEmpty(artifactId)
                && !StringUtils.isEmpty(serviceCode)) {
            PublishVersionVO example = new PublishVersionVO();
            example.setProjectId(projectId);
            example.setId(publishVersionId);
            example.setGroupId(groupId);
            example.setVersion(version);
            example.setArtifactId(artifactId);
            example.setServiceCode(serviceCode);
            if (isExisted(projectId, example)) {
                throw new CommonException("error.publish.version.duplicate");
            }
        }
        PublishVersionDTO dto = modelMapper.map(publishVersionVO, PublishVersionDTO.class);
        dto.setId(publishVersionId);
        dto.setProjectId(projectId);
        dto.setOrganizationId(ConvertUtil.getOrganizationId(projectId));
        if (publishVersionMapper.updateByPrimaryKeySelective(dto) != 1) {
            throw new CommonException("error.publish.version.update");
        }
        return modelMapper.map(publishVersionMapper.selectByPrimaryKey(publishVersionDTO.getId()), PublishVersionVO.class);
    }

    @Override
    public PublishVersionVO query(Long projectId, Long publishVersionId) {
        PublishVersionDTO dto = new PublishVersionDTO();
        dto.setId(publishVersionId);
        dto.setProjectId(projectId);
        return modelMapper.map(publishVersionMapper.selectOne(dto), PublishVersionVO.class);
    }

    @Override
    public void delete(Long projectId, Long publishVersionId) {
        PublishVersionDTO record = new PublishVersionDTO();
        record.setId(publishVersionId);
        record.setProjectId(projectId);
        if (publishVersionMapper.selectOne(record) == null) {
            throw new CommonException(PUBLISH_VERSION_NOT_EXIST_EXCEPTION);
        }
        publishVersionMapper.deleteByPrimaryKey(publishVersionId);
        publishVersionTreeClosureMapper.delete(buildTreeClosureSelf(publishVersionId, projectId, ConvertUtil.getOrganizationId(projectId)));
    }

    @Override
    public List<PublishVersionVO> parsePom(Long projectId,
                                           String groupIds,
                                           MultipartFile multipartFile,
                                           Long publishVersionId,
                                           Boolean writeBack) {
        ProjectVO project = baseFeignClient.queryProject(projectId).getBody();
        if (project == null) {
            throw new CommonException("error.project.not.existed");
        }
        validateProjectCategories(project);
        List<AppServiceRepVO> appServiceRepList =
                devopsClientOperator.listAppService(projectId, 1, 0, true);
        String fileName = multipartFile.getOriginalFilename();
        if (!fileName.endsWith(".xml")) {
            throw new CommonException("error.illegal.pom.file");
        }
        try {
            Long organizationId = ConvertUtil.getOrganizationId(projectId);
            InputStream pomInputStream = multipartFile.getInputStream();
            PublishVersionVO self = new PublishVersionVO();
            List<PublishVersionVO> publishVersionList = pomService.parse(groupIds, pomInputStream, appServiceRepList, self);
            if (Boolean.TRUE.equals(writeBack)
                    && !StringUtils.isEmpty(self.getArtifactId())
                    && !StringUtils.isEmpty(self.getGroupId())
                    && !StringUtils.isEmpty(self.getVersion())
                    && !StringUtils.isEmpty(self.getServiceCode())) {
                PublishVersionDTO dto = publishVersionMapper.selectByPrimaryKey(publishVersionId);
                if (dto == null) {
                    throw new CommonException(PUBLISH_VERSION_NOT_EXIST_EXCEPTION);
                }
                PublishVersionVO vo = modelMapper.map(dto, PublishVersionVO.class);
                vo.setGroupId(self.getGroupId());
                vo.setArtifactId(self.getArtifactId());
                vo.setVersion(self.getVersion());
                vo.setServiceCode(self.getServiceCode());
                update(projectId, publishVersionId, vo);
            }
            publishVersionList.forEach(x -> {
                x.setOrganizationId(organizationId);
                if (x.getProjectId() == null) {
                    x.setProjectId(projectId);
                }
            });
            return publishVersionList;
        } catch (IOException
                | SAXException
                | ParserConfigurationException e) {
            throw new CommonException("error.parse.pom", e);
        }
    }

    @Override
    public List<PublishVersionVO> batchCreate(Long projectId, List<PublishVersionVO> publishVersionList) {
        List<PublishVersionVO> result = new ArrayList<>();
        if(!ObjectUtils.isEmpty(publishVersionList)) {
            publishVersionList.forEach(x -> result.add(create(projectId, x)));
        }
        return result;
    }


    @Override
    public Page<PublishVersionVO> list(Long projectId, PublishVersionVO publishVersionVO, PageRequest pageRequest) {
        return PageHelper.doPageAndSort(pageRequest, () -> publishVersionMapper.listByOptions(projectId, publishVersionVO));
    }

    private void validateProjectCategories(ProjectVO project) {
        Set<String> projectCategoryCodes = new HashSet<>();
        if (!ObjectUtils.isEmpty(project.getCategories())) {
            projectCategoryCodes.addAll(
                    project
                            .getCategories()
                            .stream()
                            .map(ProjectCategoryDTO::getCode)
                            .collect(Collectors.toSet()));
        }
        if (!projectCategoryCodes.contains(ProjectCategory.MODULE_DEVOPS)) {
            throw new CommonException("error.project.category.not.contains.devops");
        }
    }

    private void createPublishVersionTreeClosure(Long id, Long projectId, Long organizationId) {
        PublishVersionTreeClosureDTO dto = buildTreeClosureSelf(id, projectId, organizationId);
        if (publishVersionTreeClosureMapper.select(dto).isEmpty()) {
            publishVersionTreeClosureMapper.insert(dto);
        }
    }

    private PublishVersionTreeClosureDTO buildTreeClosureSelf(Long id, Long projectId, Long organizationId) {
        PublishVersionTreeClosureDTO dto = new PublishVersionTreeClosureDTO();
        dto.setAncestorId(id);
        dto.setDescendantId(id);
        dto.setDescendantParent(0L);
        dto.setProjectId(projectId);
        dto.setOrganizationId(organizationId);
        return dto;
    }

    private void createValidator(PublishVersionVO publishVersionVO,
                                 String version,
                                 String groupId,
                                 String artifactId,
                                 String serviceCode) {
        boolean isAppService = Boolean.TRUE.equals(publishVersionVO.getAppService());
        if (isAppService) {
            String versionAlias = publishVersionVO.getVersionAlias();
            if (StringUtils.isEmpty(versionAlias)) {
                throw new CommonException(VERSION_ALIAS_EMPTY_EXCEPTION);
            }
        } else {
            if (StringUtils.isEmpty(version)) {
                throw new CommonException(VERSION_EMPTY_EXCEPTION);
            }
            if (StringUtils.isEmpty(groupId)) {
                throw new CommonException(GROUP_ID_EMPTY_EXCEPTION);
            }
            if (StringUtils.isEmpty(artifactId)) {
                throw new CommonException(ARTIFACT_ID_EMPTY_EXCEPTION);
            }
            if (StringUtils.isEmpty(serviceCode)) {
                throw new CommonException(SERVICE_CODE_EMPTY_EXCEPTION);
            }
        }
    }
}
