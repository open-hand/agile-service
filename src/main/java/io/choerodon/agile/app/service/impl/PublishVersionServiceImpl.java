package io.choerodon.agile.app.service.impl;

import io.choerodon.agile.api.vo.business.IssueListFieldKVVO;
import io.choerodon.agile.app.service.*;
import io.choerodon.agile.infra.dto.*;
import io.choerodon.agile.infra.dto.business.IssueDTO;
import io.choerodon.agile.infra.enums.IssueTypeCode;
import io.choerodon.agile.infra.mapper.*;
import io.choerodon.agile.infra.utils.AssertUtilsForCommonException;
import io.choerodon.agile.infra.utils.PageUtil;
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
import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;
import javax.xml.parsers.ParserConfigurationException;

import io.choerodon.agile.api.vo.*;
import io.choerodon.agile.infra.enums.ProjectCategory;
import io.choerodon.agile.infra.feign.BaseFeignClient;
import io.choerodon.agile.infra.feign.operator.DevopsClientOperator;
import io.choerodon.agile.infra.feign.vo.ProjectCategoryDTO;
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
    private BaseFeignClient baseFeignClient;
    @Autowired
    private DevopsClientOperator devopsClientOperator;
    @Autowired
    private PomService pomService;
    @Autowired
    private PublishVersionTreeClosureMapper publishVersionTreeClosureMapper;
    @Autowired
    private PublishVersionTreeService publishVersionTreeService;
    @Autowired
    private IssueService issueService;
    @Autowired
    private IssueStatusMapper issueStatusMapper;
    @Autowired
    private IssueTypeMapper issueTypeMapper;
    @Autowired(required = false)
    private AgilePluginService agilePluginService;
    @Autowired
    private IssueMapper issueMapper;
    @Autowired
    private TagIssueRelMapper tagIssueRelMapper;
    @Autowired
    private TagCompareHistoryMapper tagCompareHistoryMapper;


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
        createValidator(publishVersionVO, version, groupId, artifactId);
        publishVersionVO.setProjectId(projectId);
        publishVersionVO.setOrganizationId(ConvertUtil.getOrganizationId(projectId));
        if (!StringUtils.isEmpty(version)
                && !StringUtils.isEmpty(groupId)
                && !StringUtils.isEmpty(artifactId)
                && isExisted(projectId, publishVersionVO)) {
            throw new CommonException("error.publish.version.duplicate");
        }
        if (!StringUtils.isEmpty(publishVersionVO.getVersionAlias())
                && checkAlias(projectId, publishVersionVO.getVersionAlias(), null)) {
            throw new CommonException("error.publish.version.alias.duplicate");
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
        AssertUtilsForCommonException.notNull(projectId, "error.publish.version.projectId.null");
        AssertUtilsForCommonException.notEmpty(groupId, GROUP_ID_EMPTY_EXCEPTION);
        AssertUtilsForCommonException.notEmpty(artifactId, ARTIFACT_ID_EMPTY_EXCEPTION);
        AssertUtilsForCommonException.notEmpty(version, VERSION_EMPTY_EXCEPTION);

        PublishVersionDTO dto = new PublishVersionDTO();
        dto.setProjectId(projectId);
        dto.setGroupId(groupId);
        dto.setArtifactId(artifactId);
        dto.setVersion(version);
        List<PublishVersionDTO> list = publishVersionMapper.select(dto);
        if (publishVersionVO.getId() == null) {
            return !list.isEmpty();
        } else {
            if (list.isEmpty()) {
                return false;
            } else {
                Long id = list.get(0).getId();
                return !id.equals(publishVersionVO.getId());
            }
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
        if (!StringUtils.isEmpty(version)
                && !StringUtils.isEmpty(groupId)
                && !StringUtils.isEmpty(artifactId)) {
            PublishVersionVO example = new PublishVersionVO();
            example.setProjectId(projectId);
            example.setId(publishVersionId);
            example.setGroupId(groupId);
            example.setVersion(version);
            example.setArtifactId(artifactId);
            if (isExisted(projectId, example)) {
                throw new CommonException("error.publish.version.duplicate");
            }
        }
        if (!StringUtils.isEmpty(publishVersionVO.getVersionAlias())
                && checkAlias(projectId, publishVersionVO.getVersionAlias(), publishVersionId)) {
            throw new CommonException("error.publish.version.alias.duplicate");
        }
        PublishVersionDTO dto = modelMapper.map(publishVersionVO, PublishVersionDTO.class);
        dto.setId(publishVersionId);
        dto.setProjectId(projectId);
        dto.setOrganizationId(ConvertUtil.getOrganizationId(projectId));
        resetTagByServiceCode(dto, publishVersionDTO.getServiceCode());
        if (publishVersionMapper.updateByPrimaryKey(dto) != 1) {
            throw new CommonException("error.publish.version.update");
        }
        return modelMapper.map(publishVersionMapper.selectByPrimaryKey(publishVersionDTO.getId()), PublishVersionVO.class);
    }

    private void resetTagByServiceCode(PublishVersionDTO dto,
                                       String originalServiceCode) {
        String serviceCode = dto.getServiceCode();
        if (StringUtils.isEmpty(serviceCode)
                || !Objects.equals(serviceCode, originalServiceCode)) {
            dto.setTagName(null);
        }
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
        publishVersionTreeClosureMapper.deleteAssociatedData(publishVersionId, projectId, ConvertUtil.getOrganizationId(projectId));
        if (agilePluginService != null) {
            agilePluginService.deleteAssociatedPublishVersion(publishVersionId);
        }
        publishVersionMapper.deleteByPrimaryKey(publishVersionId);
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
    public List<PublishVersionVO> batchCreate(Long projectId,
                                              Long publishVersionId,
                                              List<PublishVersionVO> publishVersionList) {
        List<PublishVersionVO> result = new ArrayList<>();
        if (!ObjectUtils.isEmpty(publishVersionList)) {
            publishVersionList.forEach(x -> result.add(create(projectId, x)));
        }
        if (publishVersionId != null && !result.isEmpty()) {
            VersionTreeVO vo = new VersionTreeVO();
            vo.setId(publishVersionId);
            List<VersionTreeVO> children = new ArrayList<>();
            vo.setChildren(children);
            result.forEach(x -> {
                VersionTreeVO child = new VersionTreeVO();
                child.setId(x.getId());
                children.add(child);
            });
            publishVersionTreeService.add(projectId, ConvertUtil.getOrganizationId(projectId), vo);
        }
        return result;
    }


    @Override
    public Page<PublishVersionVO> list(Long projectId, PublishVersionVO publishVersionVO, PageRequest pageRequest) {
        Page<PublishVersionVO> result =
                PageHelper.doPageAndSort(pageRequest, () -> publishVersionMapper.listByOptions(projectId, publishVersionVO));
        List<PublishVersionVO> content = result.getContent();
        if (!ObjectUtils.isEmpty(content)) {
            Map<String, String> codeNameMap =
                    devopsClientOperator.listActiveAppService(projectId)
                            .stream()
                            .collect(Collectors.toMap(AppServiceRepVO::getCode, AppServiceRepVO::getName));
            content.forEach(x -> x.setAppServiceName(codeNameMap.getOrDefault(x.getServiceCode(), null)));
        }
        return result;
    }

    @Override
    public Boolean checkAlias(Long projectId, String alias, Long publishVersionId) {
        if (StringUtils.isEmpty(alias)) {
            throw new CommonException("error.publish.version.alias.empty");
        }
        PublishVersionDTO dto = new PublishVersionDTO();
        dto.setProjectId(projectId);
        dto.setVersionAlias(alias);
        List<PublishVersionDTO> list = publishVersionMapper.select(dto);
        if (publishVersionId == null) {
            return !list.isEmpty();
        } else {
            if (list.isEmpty()) {
                return false;
            } else {
                Long id = list.get(0).getId();
                return !publishVersionId.equals(id);
            }
        }
    }

    @Override
    public Page<IssueListFieldKVVO> listRelIssueByOption(Long projectId,
                                                         Long organizationId,
                                                         Long publishVersionId,
                                                         SearchVO searchVO,
                                                         PageRequest pageRequest,
                                                         String issueTypeCode) {
        PublishVersionTreeClosureDTO example = new PublishVersionTreeClosureDTO();
        example.setProjectId(projectId);
        example.setOrganizationId(organizationId);
        example.setAncestorId(publishVersionId);
        Set<Long> publishVersionIds =
                publishVersionTreeClosureMapper
                        .select(example)
                        .stream()
                        .map(PublishVersionTreeClosureDTO::getDescendantId)
                        .collect(Collectors.toSet());
        Page emptyPage = PageUtil.emptyPageInfo(pageRequest.getPage(), pageRequest.getSize());
        if (publishVersionIds.isEmpty()) {
            return emptyPage;
        }
        Set<Long> issueIds = publishVersionMapper.selectIssueIds(new HashSet<>(Arrays.asList(projectId)), organizationId, publishVersionIds);
        if (issueIds.isEmpty()) {
            return emptyPage;
        }
        addSearchParam(searchVO, issueIds, projectId, issueTypeCode);
        return issueService.listIssueWithSub(projectId, searchVO, pageRequest, organizationId);
    }

    @Override
    public void compareTag(Long projectId,
                           Long organizationId,
                           Long publishVersionId,
                           List<TagCompareVO> tagCompareList) {
        PublishVersionDTO dto = new PublishVersionDTO();
        dto.setProjectId(projectId);
        dto.setId(publishVersionId);
        PublishVersionDTO publishVersion = publishVersionMapper.selectOne(dto);
        AssertUtilsForCommonException.notNull(publishVersion, "error.publish.version.null");
        AssertUtilsForCommonException.notEmpty(tagCompareList, "error.tagCompareList.empty");

        List<TagVO> tags = new ArrayList<>();
        Set<Long> allIssueIds = new HashSet<>();
        getIssueIdsByTagsFromDevops(tags, allIssueIds, tagCompareList, projectId);

        if (!tags.isEmpty()) {
            ProjectVO program = agilePluginService.getProgram(projectId, organizationId);
            Long programId = null;
            Map<Long, IssueDTO> issueMap = new HashMap<>();
            if (program != null) {
                programId = program.getId();
                issueMap.putAll(issueMapper.selectByIds(StringUtils.join(allIssueIds, ","))
                        .stream()
                        .collect(Collectors.toMap(IssueDTO::getIssueId, Function.identity())));
            }
            for (TagVO tag : tags) {
                addTagToIssue(tag, projectId, organizationId, programId, issueMap);
                addTagCompareHistory(tag.getTagCompareVO(), projectId, organizationId);
            }
        }
    }

    private void getIssueIdsByTagsFromDevops(List<TagVO> tags,
                                             Set<Long> allIssueIds,
                                             List<TagCompareVO> tagCompareList,
                                             Long projectId) {
        for (TagCompareVO tagCompareVO : tagCompareList) {
            String appServiceCode = tagCompareVO.getAppServiceCode();
            String tagName = tagCompareVO.getSourceTag();
            try {
                Set<Long> issueIds =
                        devopsClientOperator.getIssueIdsBetweenTags(projectId,
                                tagCompareVO.getAppServiceId(),
                                tagCompareVO.getSourceTag(),
                                tagCompareVO.getTargetTag());
                if (!ObjectUtils.isEmpty(issueIds)) {
                    TagVO tagVO = new TagVO();
                    tagVO.setProjectId(projectId);
                    tagVO.setAppServiceCode(appServiceCode);
                    tagVO.setTagName(tagName);
                    tagVO.setIssueIds(issueIds);
                    tagVO.setTagCompareVO(tagCompareVO);
                    tags.add(tagVO);
                    allIssueIds.addAll(issueIds);
                }
            } catch (Exception e) {
                throw new CommonException("error.getIssue.from.devops", e);
            }
        }
    }

    private void addTagCompareHistory(TagCompareVO tagCompareVO,
                                      Long projectId,
                                      Long organizationId) {

        TagCompareHistoryDTO dto = new TagCompareHistoryDTO();
        dto.setProjectId(projectId);
        dto.setOrganizationId(organizationId);
        dto.setSource(tagCompareVO.getSourceTag());
        dto.setTarget(tagCompareVO.getTargetTag());
        dto.setAppServiceCode(tagCompareVO.getAppServiceCode());
        if (tagCompareHistoryMapper.select(dto).isEmpty()) {
            tagCompareHistoryMapper.insert(dto);
        }
    }

    private void addTagToIssue(TagVO tag,
                               Long projectId,
                               Long organizationId,
                               Long programId,
                               Map<Long, IssueDTO> issueMap) {
        String appServiceCode = tag.getAppServiceCode();
        String tagName = tag.getTagName();
        tag.getIssueIds().forEach(x -> {
            IssueDTO issue = issueMap.get(x);
            String issueTypeCode = issue.getTypeCode();
            if (issueTypeCode == null) {
                return;
            }
            TagIssueRelDTO dto = new TagIssueRelDTO();
            dto.setIssueId(x);
            dto.setProjectId(projectId);
            dto.setOrganizationId(organizationId);
            dto.setAppServiceCode(appServiceCode);
            dto.setTagName(tagName);
            if (tagIssueRelMapper.select(dto).isEmpty()) {
                tagIssueRelMapper.insert(dto);
            }
            if (IssueTypeCode.isStory(issueTypeCode)
                    && programId != null
                    && issue.getFeatureId() != null
                    && Objects.equals(0L, issue.getFeatureId() )) {
                agilePluginService.addTagToFeature(issue.getFeatureId(), programId, organizationId, appServiceCode, tagName);
            }
        });
    }

    private void addSearchParam(SearchVO searchVO,
                                Set<Long> issueIds,
                                Long projectId,
                                String typeCode) {
        Map<String, Object> otherArgs = searchVO.getOtherArgs();
        if (otherArgs == null) {
            otherArgs = new HashMap<>();
            searchVO.setOtherArgs(otherArgs);
        }
        List<String> issueIdList = (List<String>) otherArgs.get("issueIds");
        if (issueIdList == null) {
            issueIdList = new ArrayList<>();
            otherArgs.put("issueIds", issueIdList);
        }
        for (Long issueId : issueIds) {
            issueIdList.add(issueId + "");
        }
        addCompletedStatus(searchVO, projectId);
        addIssueType(searchVO, projectId, typeCode);
        resetTreeParam(searchVO);
    }

    private void resetTreeParam(SearchVO searchVO) {
        Map<String, Object> otherArgs = searchVO.getOtherArgs();
        if (otherArgs == null) {
            otherArgs = new HashMap<>();
            searchVO.setOtherArgs(otherArgs);
        }
        otherArgs.put("tree", false);
    }

    private void addIssueType(SearchVO searchVO, Long projectId, String typeCode) {
        Long organizationId = ConvertUtil.getOrganizationId(projectId);
        IssueTypeSearchVO issueTypeSearchVO = new IssueTypeSearchVO();
        issueTypeSearchVO.setTypeCodes(Arrays.asList(typeCode));
        Set<Long> issueTypeIds =
                issueTypeMapper.selectByOptions(organizationId, projectId, issueTypeSearchVO)
                        .stream()
                        .map(IssueTypeVO::getId)
                        .collect(Collectors.toSet());
        if (!issueTypeIds.isEmpty()) {
            addToAdvancedSearchArgsByKey(searchVO, "issueTypeId", issueTypeIds);
        }
    }

    private void addToAdvancedSearchArgsByKey(SearchVO searchVO, String key, Set<Long> valueSet) {
        Map<String, Object> advancedSearchArgs = searchVO.getAdvancedSearchArgs();
        if (advancedSearchArgs == null) {
            advancedSearchArgs = new HashMap<>();
            searchVO.setAdvancedSearchArgs(advancedSearchArgs);
        }
        List<String> list = (List<String>) advancedSearchArgs.get(key);
        if (list == null) {
            list = new ArrayList<>();
            advancedSearchArgs.put(key, list);
        }
        for (Long value : valueSet) {
            list.add(value + "");
        }
    }

    private void addCompletedStatus(SearchVO searchVO, Long projectId) {
        IssueStatusDTO issueStatusDTO = new IssueStatusDTO();
        issueStatusDTO.setProjectId(projectId);
        issueStatusDTO.setCompleted(true);
        issueStatusDTO.setEnable(true);
        Set<Long> statusIds =
                issueStatusMapper
                        .select(issueStatusDTO)
                        .stream()
                        .map(IssueStatusDTO::getStatusId)
                        .collect(Collectors.toSet());
        if (!statusIds.isEmpty()) {
            addToAdvancedSearchArgsByKey(searchVO, "statusId", statusIds);
        }
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
                                 String artifactId) {
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
        }
    }
}
