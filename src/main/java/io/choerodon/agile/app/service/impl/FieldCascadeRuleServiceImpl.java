package io.choerodon.agile.app.service.impl;

import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import javax.annotation.Resource;

import org.apache.commons.collections4.CollectionUtils;
import org.modelmapper.ModelMapper;
import org.modelmapper.TypeToken;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import io.choerodon.agile.api.vo.*;
import io.choerodon.agile.api.vo.business.IssueVO;
import io.choerodon.agile.api.vo.business.ProjectRelationshipInfoVO;
import io.choerodon.agile.app.service.*;
import io.choerodon.agile.infra.dto.*;
import io.choerodon.agile.infra.dto.business.IssueDetailDTO;
import io.choerodon.agile.infra.enums.FieldCode;
import io.choerodon.agile.infra.enums.FieldType;
import io.choerodon.agile.infra.enums.ObjectSchemeCode;
import io.choerodon.agile.infra.enums.PageCode;
import io.choerodon.agile.infra.feign.BaseFeignClient;
import io.choerodon.agile.infra.mapper.*;
import io.choerodon.agile.infra.utils.ConvertUtil;
import io.choerodon.agile.infra.utils.EncryptionUtils;
import io.choerodon.agile.infra.utils.PageUtil;
import io.choerodon.core.domain.Page;
import io.choerodon.core.exception.CommonException;
import io.choerodon.core.utils.PageUtils;
import io.choerodon.mybatis.pagehelper.PageHelper;
import io.choerodon.mybatis.pagehelper.domain.PageRequest;

/**
 * @author chihao.ran@hand-china.com
 * 2021/07/13 16:06
 */
@Service
@Transactional(rollbackFor = Exception.class)
public class FieldCascadeRuleServiceImpl implements FieldCascadeRuleService {

    private static final String MEMBER = "member";
    private static final String PRIORITY = "priority";
    private static final String COMPONENT = "component";
    private static final String VERSION = "version";
    private static final String CUSTOM = "custom";
    private static final String OTHER = "other";
    private static final String SUB_PROJECT = "subProject";
    private static final String ENVIRONMENT = "environment";

    @Resource
    private FieldCascadeRuleMapper fieldCascadeRuleMapper;
    @Resource
    private FieldCascadeRuleOptionMapper fieldCascadeRuleOptionMapper;
    @Resource
    private IssueComponentMapper issueComponentMapper;
    @Autowired
    private PriorityService priorityService;
    @Resource
    private ProductVersionMapper productVersionMapper;
    @Autowired
    private ModelMapper modelMapper;
    @Autowired
    private ObjectSchemeFieldService objectSchemeFieldService;
    @Autowired
    private UserService userService;
    @Autowired
    private FieldOptionMapper fieldOptionMapper;
    @Resource
    private ObjectSchemeFieldMapper objectSchemeFieldMapper;
    @Autowired
    private BaseFeignClient baseFeignClient;
    @Resource
    private IssueMapper issueMapper;
    @Autowired(required = false)
    private AgilePluginService agilePluginService;
    @Autowired(required = false)
    private BacklogExpandService backlogExpandService;

    private static final Set<String> CANT_CASCADE_FIELD_CODE = Stream.of(
            FieldCode.ISSUE_TYPE,
            FieldCode.SUMMARY,
            FieldCode.DESCRIPTION,
            FieldCode.REMAINING_TIME,
            FieldCode.STORY_POINTS,
            FieldCode.STATUS,
            FieldCode.CREATOR,
            FieldCode.UPDATOR,
            FieldCode.CREATION_DATE,
            FieldCode.LAST_UPDATE_DATE,
            FieldCode.TAG,
            FieldCode.EPIC_NAME,
            FieldCode.TIME_TRACE,
            FieldCode.FEATURE_TYPE,
            FieldCode.PROGRAM_VERSION,
            FieldCode.EPIC,
            FieldCode.SPRINT,
            FieldCode.LABEL,
            FieldCode.FEATURE,
            FieldCode.PI,
            FieldCode.ENVIRONMENT,
            FieldCode.BELONG_TO_BACKLOG,
            FieldCode.PROGRESS_FEEDBACK,
            FieldCode.EMAIL,
            FieldCode.PROCESSOR,
            FieldCode.PRODUCT
    ).collect(Collectors.toSet());

    @Override
    public FieldCascadeRuleVO createFieldCascadeRule(Long projectId, FieldCascadeCreateVO fieldCascadeCreate) {
        Long organizationId = ConvertUtil.getOrganizationId(projectId);
        validCreateField(fieldCascadeCreate);
        validExit(fieldCascadeCreate, projectId);

        FieldCascadeRuleDTO fieldCascadeRule = modelMapper.map(fieldCascadeCreate, FieldCascadeRuleDTO.class);
        fieldCascadeRule.setProjectId(projectId);
        fieldCascadeRule.setOrganizationId(organizationId);
        validLoop(fieldCascadeRule);
        fieldCascadeRuleMapper.insertSelective(fieldCascadeRule);
        if (!CollectionUtils.isEmpty(fieldCascadeCreate.getFieldCascadeRuleOptionList())) {
            Long cascadeFieldId = fieldCascadeCreate.getCascadeFieldId();
            ObjectSchemeFieldDTO objectSchemeFieldDTO = objectSchemeFieldMapper.selectByPrimaryKey(cascadeFieldId);
            fieldCascadeCreate.getFieldCascadeRuleOptionList().forEach(fieldCascadeRuleOptionVO -> {
                FieldCascadeRuleOptionDTO fieldCascadeRuleOptionDTO = modelMapper.map(fieldCascadeRuleOptionVO, FieldCascadeRuleOptionDTO.class);
                if (Objects.equals(FieldCode.SUB_PROJECT, objectSchemeFieldDTO.getCode())) {
                    fieldCascadeRuleOptionDTO.setCascadeOptionId(fieldCascadeRuleOptionVO.getProjectId());
                }
                fieldCascadeRuleOptionDTO.setFieldCascadeRuleId(fieldCascadeRule.getId());
                fieldCascadeRuleOptionDTO.setProjectId(projectId);
                fieldCascadeRuleOptionDTO.setOrganizationId(organizationId);
                baseCreateFieldCascadeRuleOption(fieldCascadeRuleOptionDTO);
            });
        }
        return modelMapper.map(fieldCascadeRule, FieldCascadeRuleVO.class);
    }

    private void validCreateField(FieldCascadeCreateVO fieldCascadeCreate) {
        if (fieldCascadeCreate.getIssueTypeId() == null) {
            throw new CommonException("error.fieldCascadeCreate.issueType.null");
        }
        if (fieldCascadeCreate.getFieldId() == null) {
            throw new CommonException("error.fieldCascadeCreate.fieldId.null");
        }
        if (fieldCascadeCreate.getCascadeFieldId() == null) {
            throw new CommonException("error.fieldCascadeCreate.cascadeFieldId.null");
        }
        if (fieldCascadeCreate.getFieldOptionId() == null) {
            throw new CommonException("error.fieldCascadeCreate.fieldOption.null");
        }
    }

    private void validExit(FieldCascadeCreateVO fieldCascadeCreate, Long projectId) {
        FieldCascadeRuleDTO fieldCascadeRuleRecord = new FieldCascadeRuleDTO();
        fieldCascadeRuleRecord.setIssueTypeId(fieldCascadeCreate.getIssueTypeId());
        fieldCascadeRuleRecord.setFieldId(fieldCascadeCreate.getFieldId());
        fieldCascadeRuleRecord.setFieldOptionId(fieldCascadeCreate.getFieldOptionId());
        fieldCascadeRuleRecord.setCascadeFieldId(fieldCascadeCreate.getCascadeFieldId());
        fieldCascadeRuleRecord.setProjectId(projectId);
        if (fieldCascadeRuleMapper.selectCount(fieldCascadeRuleRecord) > 0) {
            throw new CommonException("error.fieldCascadeRule.exist");
        }
    }

    private void baseCreateFieldCascadeRuleOption(FieldCascadeRuleOptionDTO fieldCascadeRuleOptionDTO) {
        if (fieldCascadeRuleOptionMapper.insertSelective(fieldCascadeRuleOptionDTO) != 1) {
            throw new CommonException("error.fieldCascadeRuleOption.create");
        }
    }

    private void baseUpdateFieldCascadeRuleOption(FieldCascadeRuleOptionDTO fieldCascadeRuleOptionDTO) {
        if (fieldCascadeRuleOptionMapper.updateByPrimaryKeySelective(fieldCascadeRuleOptionDTO) != 1) {
            throw new CommonException("error.fieldCascadeRuleOption.create");
        }
    }

    @Override
    public List<FieldCascadeRuleVO> listFieldCascadeRuleByIssueType(Long projectId, Long issueTypeId, Long fieldId) {
        Long organizationId = ConvertUtil.getOrganizationId(projectId);
        List<FieldCascadeRuleVO> result = fieldCascadeRuleMapper.listFieldCascadeRuleByIssueType(projectId, issueTypeId, fieldId);
        processDefaultValue(projectId, organizationId, result);
        return result;
    }

    @Override
    public FieldCascadeRuleVO updateFieldCascadeRule(Long projectId, Long fieldCascadeRuleId, FieldCascadeUpdateVO fieldCascadeUpdate) {
        FieldCascadeRuleDTO fieldCascadeRuleDTO = modelMapper.map(fieldCascadeUpdate, FieldCascadeRuleDTO.class);
        fieldCascadeRuleDTO.setProjectId(projectId);
        fieldCascadeRuleDTO.setId(fieldCascadeRuleId);
        if (fieldCascadeRuleMapper.updateOptional(fieldCascadeRuleDTO, "hidden", "required", "defaultValue") != 1) {
            throw new CommonException("error.fieldCascadeRule.update");
        }
        updateAndDeleteFieldCascadeRuleOption(
                fieldCascadeUpdate.getFieldCascadeRuleOptionList(),
                fieldCascadeRuleId,
                projectId);
        return modelMapper.map(fieldCascadeRuleDTO, FieldCascadeRuleVO.class);
    }

    @Override
    public FieldCascadeRuleVO fieldCascadeRuleDetail(Long projectId, Long fieldCascadeRuleId) {
        Long organizationId = ConvertUtil.getOrganizationId(projectId);
        FieldCascadeRuleVO fieldCascadeRuleVO = fieldCascadeRuleMapper.selectFieldCascadeRuleDetail(projectId, fieldCascadeRuleId);
        if (fieldCascadeRuleVO == null) {
            return null;
        }
        FieldCascadeRuleOptionDTO fieldCascadeRuleOptionRecord = new FieldCascadeRuleOptionDTO();
        fieldCascadeRuleOptionRecord.setFieldCascadeRuleId(fieldCascadeRuleId);
        fieldCascadeRuleOptionRecord.setProjectId(projectId);
        List<FieldCascadeRuleOptionDTO> fieldCascadeRuleOptionList = fieldCascadeRuleOptionMapper.select(fieldCascadeRuleOptionRecord);
        if (!CollectionUtils.isEmpty(fieldCascadeRuleOptionList)) {
            fieldCascadeRuleVO.setFieldCascadeRuleOptionList(modelMapper.map(fieldCascadeRuleOptionList, new TypeToken<List<FieldCascadeRuleOptionVO>>() {
            }.getType()));
        }
        processDefaultValue(projectId, organizationId, Stream.of(fieldCascadeRuleVO).collect(Collectors.toList()));
        return fieldCascadeRuleVO;
    }

    @Override
    public List<PageConfigFieldVO> listCascadePageFieldView(Long projectId, Long issueTypeId, Long fieldId) {
        Long organizationId = ConvertUtil.getOrganizationId(projectId);
        Set<Long> previousFieldIds = getRelPreviousFieldIds(fieldId, issueTypeId, projectId);
        previousFieldIds.add(fieldId);
        List<PageConfigFieldVO> pageConfigFields = objectSchemeFieldService.queryPageConfigFields(organizationId, projectId, issueTypeId);
        return pageConfigFields.stream()
                .filter(pageConfigFieldVO ->
                        !previousFieldIds.contains(pageConfigFieldVO.getFieldId())
                                && !CANT_CASCADE_FIELD_CODE.contains(pageConfigFieldVO.getFieldCode()))
                .collect(Collectors.toList());
    }

    @Override
    public List<FieldCascadeRuleVO> batchMutationFieldCascadeRule(Long projectId, List<FieldCascadeRuleVO> fieldCascadeRuleList) {
        if (CollectionUtils.isEmpty(fieldCascadeRuleList)) {
            return new ArrayList<>();
        }

        List<FieldCascadeCreateVO> createList = new ArrayList<>();
        List<FieldCascadeRuleVO> updateList = new ArrayList<>();
        List<Long> deleteIdList = new ArrayList<>();

        fieldCascadeRuleList.forEach(fieldCascadeRule -> {
            if (fieldCascadeRule.get_status() == null) {
                return;
            }
            switch (fieldCascadeRule.get_status()) {
                case create:
                    FieldCascadeCreateVO fieldCascadeCreateVO = modelMapper.map(fieldCascadeRule, FieldCascadeCreateVO.class);
                    createList.add(fieldCascadeCreateVO);
                    break;
                case update:
                    updateList.add(fieldCascadeRule);
                    break;
                case delete:
                    deleteIdList.add(fieldCascadeRule.getId());
                    break;
                default:
                    break;
            }
        });

        batchDeleteFieldCascadeRule(deleteIdList, projectId);
        batchUpdateFieldCascadeRule(updateList, projectId);
        batchCreateFieldCascadeRule(createList, projectId);
        return fieldCascadeRuleList;
    }

    @Override
    public Object listCascadeFieldOption(Long projectId,
                                         Long cascadeFieldId,
                                         CascadeFieldOptionSearchVO cascadeFieldOptionSearchVO,
                                         PageRequest pageRequest) {
        ObjectSchemeFieldDTO objectSchemeField = objectSchemeFieldMapper.selectByPrimaryKey(cascadeFieldId);
        if (objectSchemeField == null) {
            throw new CommonException("error.field.null");
        }
        Object result = null;
        Long organizationId = ConvertUtil.getOrganizationId(projectId);
        if (!CollectionUtils.isEmpty(cascadeFieldOptionSearchVO.getFieldCascadeRuleIds())) {
            cascadeFieldOptionSearchVO.setFieldCascadeRuleIds(
                    fieldCascadeRuleMapper.selectFieldCascadeRuleIdsHasOption(cascadeFieldOptionSearchVO.getFieldCascadeRuleIds(), projectId));
        }
        String optionType = getOptionType(objectSchemeField);
        switch (optionType) {
            case COMPONENT:
                result = PageHelper.doPage(
                        pageRequest, () -> fieldCascadeRuleOptionMapper.selectCascadeFieldComponent(
                                projectId,
                                cascadeFieldOptionSearchVO.getFieldCascadeRuleIds(),
                                cascadeFieldOptionSearchVO.getSearchParam()
                        ));
                break;
            case PRIORITY:
                result = PageHelper.doPage(
                        pageRequest, () -> fieldCascadeRuleOptionMapper.selectCascadeFieldPriority(
                                projectId,
                                organizationId,
                                cascadeFieldOptionSearchVO.getFieldCascadeRuleIds(),
                                cascadeFieldOptionSearchVO.getSearchParam()
                        ));
                break;
            case MEMBER:
                result = listMemberCascadeFieldOption(projectId, cascadeFieldOptionSearchVO, pageRequest);
                break;
            case CUSTOM:
                result = listCustomFieldOption(projectId, organizationId, objectSchemeField, cascadeFieldOptionSearchVO, pageRequest);
                break;
            case VERSION:
                result = PageHelper.doPage(
                        pageRequest, () -> fieldCascadeRuleOptionMapper.selectCascadeFieldVersion(
                                projectId,
                                organizationId,
                                cascadeFieldOptionSearchVO.getFieldCascadeRuleIds(),
                                cascadeFieldOptionSearchVO.getExtendParams()
                        ));
                break;
            case SUB_PROJECT:
                result = listProjectCascadeFieldOption(projectId, organizationId, objectSchemeField, cascadeFieldOptionSearchVO, pageRequest);
                break;
            case FieldCode.BACKLOG_TYPE:
            case FieldCode.BACKLOG_CLASSIFICATION:
            case FieldCode.URGENT:
                if (backlogExpandService != null) {
                    result = backlogExpandService.listBacklogFieldOption(optionType, organizationId, projectId, pageRequest, cascadeFieldOptionSearchVO);
                }
                break;
            default:
                break;
        }

        if (result == null) {
            return PageUtil.emptyPage(pageRequest.getPage(), pageRequest.getSize());
        }
        return result;
    }

    @Override
    public void filterPageFieldView(Long organizationId, Long projectId, PageFieldViewParamVO paramDTO, Long instanceId, List<PageFieldViewVO> pageFieldViews) {
        if (!ObjectSchemeCode.AGILE_ISSUE.equals(paramDTO.getSchemeCode())
                || PageCode.AGILE_ISSUE_CREATE.equals(paramDTO.getPageCode())
                || instanceId == null) {
            return;
        }
        List<FieldCascadeRuleVO> fieldCascadeRuleList = fieldCascadeRuleMapper.selectFieldCascadeRequiredOrHiddenRule(projectId, paramDTO.getIssueTypeId());
        if (CollectionUtils.isEmpty(fieldCascadeRuleList)) {
            return;
        }
        Set<Long> hiddenFieldIds = new HashSet<>();
        Set<Long> requiredIds = new HashSet<>();
        Set<Long> influenceVersionIds = new HashSet<>();
        Set<Long> fixVersionIds = new HashSet<>();
        Set<Long> componentIds = new HashSet<>();
        Set<Long> programVersionIds = new HashSet<>();

        IssueDetailDTO issueDetailDTO = getIssueDetail(organizationId, projectId, instanceId, componentIds, influenceVersionIds, fixVersionIds, programVersionIds);
        Map<Long, PageFieldViewVO> pageFieldViewMap = pageFieldViews
                .stream()
                .filter(pageFieldViewVO -> !Boolean.TRUE.equals(pageFieldViewVO.getSystem())
                        && Boolean.TRUE.equals(FieldType.hasOption(pageFieldViewVO.getFieldType())))
                .collect(Collectors.toMap(PageFieldViewVO::getFieldId, Function.identity()));

        fieldCascadeRuleList.forEach(fieldCascadeRuleVO -> {
            switch (fieldCascadeRuleVO.getFieldCode()) {
                case FieldCode.PRIORITY:
                    processHiddenAndRequiredById(
                            issueDetailDTO.getPriorityId(),
                            fieldCascadeRuleVO,
                            hiddenFieldIds, requiredIds);
                    break;
                case FieldCode.COMPONENT:
                    processHiddenAndRequiredByIds(
                            componentIds,
                            fieldCascadeRuleVO,
                            hiddenFieldIds, requiredIds);
                    break;
                case FieldCode.INFLUENCE_VERSION:
                    processHiddenAndRequiredByIds(
                            influenceVersionIds,
                            fieldCascadeRuleVO,
                            hiddenFieldIds, requiredIds);
                    break;
                case FieldCode.FIX_VERSION:
                    processHiddenAndRequiredByIds(
                            fixVersionIds,
                            fieldCascadeRuleVO,
                            hiddenFieldIds, requiredIds);
                    break;
                case FieldCode.PROGRAM_VERSION:
                    processHiddenAndRequiredByIds(
                            programVersionIds,
                            fieldCascadeRuleVO,
                            hiddenFieldIds, requiredIds);
                    break;
                default:
                    processHiddenAndRequiredByCustomValue(pageFieldViewMap, fieldCascadeRuleVO, hiddenFieldIds, requiredIds);
                    break;
            }
        });
        removeHiddenFieldAndSetRequired(hiddenFieldIds, requiredIds, pageFieldViews);
    }

    @Override
    public List<FieldCascadeRuleOptionVO> listFieldCascadeRuleOptionByRule(Long projectId, Long fieldCascadeRuleId) {
        Long organizationId = ConvertUtil.getOrganizationId(projectId);
        FieldCascadeRuleVO fieldCascadeRuleVO = fieldCascadeRuleMapper.selectFieldCascadeRuleDetail(projectId, fieldCascadeRuleId);

        FieldCascadeRuleOptionDTO fieldCascadeRuleOptionRecord = new FieldCascadeRuleOptionDTO();
        fieldCascadeRuleOptionRecord.setFieldCascadeRuleId(fieldCascadeRuleId);
        fieldCascadeRuleOptionRecord.setProjectId(projectId);
        List<FieldCascadeRuleOptionDTO> fieldCascadeRuleOptionList = fieldCascadeRuleOptionMapper.select(fieldCascadeRuleOptionRecord);
        if (CollectionUtils.isEmpty(fieldCascadeRuleOptionList)) {
            return new ArrayList<>();
        }
        List<FieldCascadeRuleOptionVO> result = modelMapper.map(fieldCascadeRuleOptionList, new TypeToken<List<FieldCascadeRuleOptionVO>>() {
        }.getType());

        String optionType = getOptionType(fieldCascadeRuleVO);
        Map<Long, String> optionNameMap = getOptionNameMapByOptionType(result, optionType, projectId, organizationId, fieldCascadeRuleVO.getCascadeFieldId());

        if (optionNameMap == null) {
            return new ArrayList<>();
        }
        Iterator<FieldCascadeRuleOptionVO> optionIterator = result.iterator();
        while (optionIterator.hasNext()) {
            FieldCascadeRuleOptionVO fieldCascadeRuleOption = optionIterator.next();
            Long optionId = fieldCascadeRuleOption.getCascadeOptionId();
            if (optionNameMap.get(optionId) == null) {
                optionIterator.remove();
            } else {
                fieldCascadeRuleOption.setCascadeOptionName(optionNameMap.get(optionId));
            }
        }
        return result;
    }

    private Map<Long, String> getOptionNameMapByOptionType(List<FieldCascadeRuleOptionVO> result,
                                                           String optionType,
                                                           Long projectId,
                                                           Long organizationId,
                                                           Long fieldId) {
        Map<Long, String> optionNameMap = null;
        switch (optionType) {
            case PRIORITY:
                List<PriorityVO> priorityList = priorityService.queryByOrganizationIdList(organizationId);
                optionNameMap = priorityList.stream().collect(Collectors.toMap(PriorityVO::getId, PriorityVO::getName));
                break;
            case COMPONENT:
                List<ComponentForListDTO> componentList = issueComponentMapper.queryComponentWithIssueNum(projectId, null, true);
                optionNameMap = componentList.stream().collect(Collectors.toMap(ComponentForListDTO::getComponentId, ComponentForListDTO::getName));
                break;
            case MEMBER:
                List<UserDTO> userDTOS = baseFeignClient.listUsersByIds(result.stream().map(FieldCascadeRuleOptionVO::getCascadeOptionId).toArray(Long[]::new), false).getBody();
                if (!CollectionUtils.isEmpty(userDTOS)) {
                    optionNameMap = userDTOS.stream().collect(Collectors.toMap(UserDTO::getId, UserDTO::getRealName));
                }
                break;
            case CUSTOM:
                List<FieldOptionDTO> fieldOptionList = fieldOptionMapper.selectByFieldId(organizationId, fieldId);
                optionNameMap = fieldOptionList.stream().collect(Collectors.toMap(FieldOptionDTO::getId, FieldOptionDTO::getValue));
                break;
            case VERSION:
                List<ProductVersionNameDTO> productVersionNameList = productVersionMapper.queryNameByOptions(projectId, null);
                optionNameMap = productVersionNameList.stream().collect(Collectors.toMap(ProductVersionNameDTO::getVersionId, ProductVersionNameDTO::getName));
                break;
            case SUB_PROJECT:
                List<ProjectVO> projectVOList = baseFeignClient.queryByIds(result.stream().map(FieldCascadeRuleOptionVO::getCascadeOptionId).collect(Collectors.toSet())).getBody();
                if (!CollectionUtils.isEmpty(projectVOList)) {
                    optionNameMap = projectVOList.stream().collect(Collectors.toMap(ProjectVO::getId, ProjectVO::getName));
                }
                break;
            case FieldCode.BACKLOG_TYPE:
            case FieldCode.BACKLOG_CLASSIFICATION:
            case FieldCode.URGENT:
                if (backlogExpandService != null) {
                    optionNameMap = backlogExpandService.getOptionNameMapByOptionType(optionType, organizationId, projectId);
                }
                break;
            default:
                break;
        }
        return optionNameMap;
    }

    private void removeHiddenFieldAndSetRequired(Set<Long> hiddenFieldIds, Set<Long> requiredIds, List<PageFieldViewVO> pageFieldViews) {
        Iterator<PageFieldViewVO> pageFieldViewIterator = pageFieldViews.iterator();
        while (pageFieldViewIterator.hasNext()) {
            PageFieldViewVO pageFieldViewVO = pageFieldViewIterator.next();
            if (requiredIds.contains(pageFieldViewVO.getFieldId())) {
                pageFieldViewVO.setRequired(true);
            }
            if (!Boolean.TRUE.equals(pageFieldViewVO.getRequired())
                    && hiddenFieldIds.contains(pageFieldViewVO.getFieldId())) {
                pageFieldViewIterator.remove();
            }
        }
    }

    private void processHiddenAndRequiredByCustomValue(Map<Long, PageFieldViewVO> pageFieldViewMap, FieldCascadeRuleVO fieldCascadeRuleVO, Set<Long> hiddenFieldIds, Set<Long> requiredIds) {
        PageFieldViewVO pageFieldView = pageFieldViewMap.get(fieldCascadeRuleVO.getFieldId());
        if (pageFieldView == null || pageFieldView.getValue() == null) {
            return;
        }

        if (pageFieldView.getValue() instanceof List) {
            Set<Long> ids = new HashSet<>(EncryptionUtils.decryptList((List<String>) pageFieldView.getValue(), EncryptionUtils.BLANK_KEY, null));
            processHiddenAndRequiredByIds(ids, fieldCascadeRuleVO, hiddenFieldIds, requiredIds);
        } else {
            Long id = EncryptionUtils.decrypt((String) pageFieldView.getValue(), "");
            processHiddenAndRequiredById(id, fieldCascadeRuleVO, hiddenFieldIds, requiredIds);
        }
    }

    private IssueDetailDTO getIssueDetail(Long organizationId, Long projectId, Long instanceId, Set<Long> componentIds, Set<Long> influenceVersionIds, Set<Long> fixVersionIds, Set<Long> programVersionIds) {
        IssueDetailDTO issueDetailDTO = issueMapper.queryIssueDetail(projectId, instanceId);
        Optional.ofNullable(issueDetailDTO.getComponentIssueRelDTOList())
                .ifPresent(componentList -> componentList.forEach(component -> componentIds.add(component.getId())));
        Optional.ofNullable(issueDetailDTO.getVersionIssueRelDTOList())
                .ifPresent(versionList -> versionList.forEach(version -> {
                    if ("fix".equals(version.getRelationType())) {
                        fixVersionIds.add(version.getVersionId());
                    } else if ("influence".equals(version.getRelationType())) {
                        influenceVersionIds.add(version.getVersionId());
                    }
                }));
        if (agilePluginService != null) {
            agilePluginService.setBusinessAttributes(issueDetailDTO);
            IssueVO issueVO = modelMapper.map(issueDetailDTO, IssueVO.class);
            agilePluginService.businessIssueDetailDTOToVO(organizationId, issueVO, issueDetailDTO, new HashMap<>(), new HashMap<>(), new HashMap<>());
            Optional.ofNullable(issueVO.getProgramVersionFeatureRelVOS())
                    .ifPresent(programVersionList -> programVersionList.forEach(
                            programVersion -> programVersionIds.add(programVersion.getProgramVersionId())));
        }
        return issueDetailDTO;
    }

    private void processHiddenAndRequiredByIds(Set<Long> issueOptionIds, FieldCascadeRuleVO fieldCascadeRule, Set<Long> hiddenFieldIds, Set<Long> requiredIds) {
        if (!issueOptionIds.contains(fieldCascadeRule.getFieldOptionId())) {
            return;
        }

        if (Boolean.TRUE.equals(fieldCascadeRule.getRequired())) {
            requiredIds.add(fieldCascadeRule.getCascadeFieldId());
        } else if (Boolean.TRUE.equals(fieldCascadeRule.getHidden())) {
            hiddenFieldIds.add(fieldCascadeRule.getCascadeFieldId());
        }
    }

    private void processHiddenAndRequiredById(Long issueOptionId, FieldCascadeRuleVO fieldCascadeRule, Set<Long> hiddenFieldIds, Set<Long> requiredIds) {
        if (!fieldCascadeRule.getFieldOptionId().equals(issueOptionId)) {
            return;
        }

        if (Boolean.TRUE.equals(fieldCascadeRule.getRequired())) {
            requiredIds.add(fieldCascadeRule.getCascadeFieldId());
        } else if (Boolean.TRUE.equals(fieldCascadeRule.getHidden())) {
            hiddenFieldIds.add(fieldCascadeRule.getCascadeFieldId());
        }
    }

    private String getOptionType(FieldCascadeRuleVO fieldCascadeRuleVO) {
        ObjectSchemeFieldDTO objectSchemeFieldDTO = new ObjectSchemeFieldDTO();
        objectSchemeFieldDTO.setFieldType(fieldCascadeRuleVO.getCascadeFieldType());
        objectSchemeFieldDTO.setCode(fieldCascadeRuleVO.getCascadeFieldCode());
        objectSchemeFieldDTO.setSystem(fieldCascadeRuleVO.getCascadeFieldSystem());
        return getOptionType(objectSchemeFieldDTO);
    }

    private String getOptionType(ObjectSchemeFieldDTO objectSchemeField) {
        if (FieldType.MEMBER.equals(objectSchemeField.getFieldType())
                || FieldType.MULTI_MEMBER.equals(objectSchemeField.getFieldType())) {
            return MEMBER;
        }
        switch (objectSchemeField.getCode()) {
            case FieldCode.COMPONENT:
                return COMPONENT;
            case FieldCode.PRIORITY:
                return PRIORITY;
            case FieldCode.FIX_VERSION:
            case FieldCode.INFLUENCE_VERSION:
                return VERSION;
            case FieldCode.SUB_PROJECT:
                return SUB_PROJECT;
            case FieldCode.ENVIRONMENT:
                return ENVIRONMENT;
            case FieldCode.BACKLOG_TYPE:
                return FieldCode.BACKLOG_TYPE;
            case FieldCode.BACKLOG_CLASSIFICATION:
                return FieldCode.BACKLOG_CLASSIFICATION;
            case FieldCode.URGENT:
                return FieldCode.URGENT;
            default:
                break;
        }
        if (!Boolean.TRUE.equals(objectSchemeField.getSystem())
                && Boolean.TRUE.equals(FieldType.hasOption(objectSchemeField.getFieldType()))) {
            return CUSTOM;
        }
        return OTHER;
    }

    private Object listCustomFieldOption(Long projectId, Long organizationId, ObjectSchemeFieldDTO objectSchemeField, CascadeFieldOptionSearchVO cascadeFieldOptionSearchVO, PageRequest pageRequest) {
        Set<Long> selected = Optional.ofNullable(cascadeFieldOptionSearchVO.getSelected()).map(HashSet::new).orElse(new HashSet<>());
        List<Long> fieldCascadeRuleIds = cascadeFieldOptionSearchVO.getFieldCascadeRuleIds();

        Page<FieldOptionVO> optionPage = PageHelper.doPage(
                pageRequest, () -> fieldCascadeRuleOptionMapper.selectCascadeFieldCustom(organizationId, projectId, objectSchemeField.getId(), cascadeFieldOptionSearchVO.getSearchParam(), selected, fieldCascadeRuleIds));

        if (CollectionUtils.isNotEmpty(selected) && pageRequest.getPage() == 0) {
            List<FieldOptionVO> selectedOption = fieldCascadeRuleOptionMapper.selectCascadeFieldCustomByOptionIds(organizationId, projectId, selected, fieldCascadeRuleIds);
            if (CollectionUtils.isNotEmpty(selectedOption)) {
                selectedOption.addAll(optionPage.getContent());
                optionPage.setContent(selectedOption);
            }
        }
        return optionPage;
    }

    private Page<ProjectRelationshipInfoVO> listProjectCascadeFieldOption(Long projectId, Long organizationId, ObjectSchemeFieldDTO objectSchemeField, CascadeFieldOptionSearchVO cascadeFieldOptionSearchVO, PageRequest pageRequest) {
        if (agilePluginService == null) {
            return null;
        }
        List<ProjectRelationshipInfoVO> projectRelationshipInfoVOS = agilePluginService.getProjUnderGroup(organizationId, projectId, projectId, true);
        if (CollectionUtils.isEmpty(projectRelationshipInfoVOS)) {
            return null;
        }

        Set<Long> visibleOptionIds = new HashSet<>();
        if (!CollectionUtils.isEmpty(cascadeFieldOptionSearchVO.getFieldCascadeRuleIds())) {
            visibleOptionIds.addAll(fieldCascadeRuleOptionMapper.selectVisibleOptionIds(projectId, cascadeFieldOptionSearchVO.getFieldCascadeRuleIds()));
        }
        List<ProjectRelationshipInfoVO> result = projectRelationshipInfoVOS.stream().filter(projectRelationshipInfoVO -> visibleOptionIds.contains(projectRelationshipInfoVO.getProjectId())).collect(Collectors.toList());
        return PageUtils.createPageFromList(result, pageRequest);
    }

    private Page<UserDTO> listMemberCascadeFieldOption(Long projectId, CascadeFieldOptionSearchVO cascadeFieldOptionSearchVO, PageRequest pageRequest) {

        Page<UserDTO> result;
        if (CollectionUtils.isEmpty(cascadeFieldOptionSearchVO.getFieldCascadeRuleIds())) {
            Set<Long> visibleOptionIds = fieldCascadeRuleOptionMapper.selectVisibleOptionIds(projectId, cascadeFieldOptionSearchVO.getFieldCascadeRuleIds());
            AgileUserVO agileUserVO = new AgileUserVO(visibleOptionIds, null, cascadeFieldOptionSearchVO.getSearchParam(), null, null);
            result = baseFeignClient.agileUsers(
                    projectId,
                    pageRequest.getPage(), pageRequest.getSize(),
                    agileUserVO).getBody();
        } else {
            result = baseFeignClient.listUsersByProjectId(
                    projectId,
                    pageRequest.getPage(), pageRequest.getSize(),
                    cascadeFieldOptionSearchVO.getSearchParam()).getBody();
        }
        return result;
    }

    private void batchDeleteFieldCascadeRule(List<Long> deleteIdList, Long projectId) {
        if (CollectionUtils.isEmpty(deleteIdList)) {
            return;
        }
        fieldCascadeRuleOptionMapper.batchDeleteByFieldCascadeRuleIds(deleteIdList, projectId);
        fieldCascadeRuleMapper.batchDeleteByIds(deleteIdList, projectId);
    }

    private void batchUpdateFieldCascadeRule(List<FieldCascadeRuleVO> updateList, Long projectId) {
        if (!CollectionUtils.isEmpty(updateList)) {
            updateList.forEach(updateFieldCascadeRule -> {
                FieldCascadeUpdateVO fieldCascadeUpdateVO = modelMapper.map(updateFieldCascadeRule, FieldCascadeUpdateVO.class);
                updateFieldCascadeRule(projectId, updateFieldCascadeRule.getId(), fieldCascadeUpdateVO);
            });
        }
    }

    private void batchCreateFieldCascadeRule(List<FieldCascadeCreateVO> createList, Long projectId) {
        if (!CollectionUtils.isEmpty(createList)) {
            createList.forEach(fieldCascadeCreateVO -> createFieldCascadeRule(projectId, fieldCascadeCreateVO));
        }
    }

    private void processDefaultValue(Long projectId, Long organizationId, List<FieldCascadeRuleVO> fieldCascadeRuleList) {
        if (CollectionUtils.isEmpty(fieldCascadeRuleList)) {
            return;
        }
        Map<Long, UserMessageDTO> userMap = getDefaultUserMap(fieldCascadeRuleList);
        fieldCascadeRuleList.forEach(fieldCascadeRuleVO -> {
            if (CollectionUtils.isEmpty(fieldCascadeRuleVO.getFieldCascadeRuleOptionList())
                    || fieldCascadeRuleVO.getFieldCascadeRuleOptionList().get(0).getId() == null) {
                fieldCascadeRuleVO.setFieldCascadeRuleOptionList(new ArrayList<>());
                return;
            }

            if (FieldType.MEMBER.equals(fieldCascadeRuleVO.getCascadeFieldType())
                    || FieldType.MULTI_MEMBER.equals(fieldCascadeRuleVO.getCascadeFieldType())) {
                setDefaultUserInfo(fieldCascadeRuleVO, userMap);
            }
            switch (fieldCascadeRuleVO.getCascadeFieldCode()) {
                //多选
                case FieldCode.COMPONENT:
                    List<IssueComponentVO> issueComponentList = modelMapper.map(issueComponentMapper.selectByProjectId(projectId), new TypeToken<List<IssueComponentVO>>() {
                    }.getType());
                    Map<Long, Object> issueComponentMap = issueComponentList.stream().collect(Collectors.toMap(IssueComponentVO::getComponentId, Function.identity()));
                    setDefaultValueObjsOfMultiple(issueComponentMap, fieldCascadeRuleVO);
                    break;
                case FieldCode.PRIORITY:
                    List<PriorityVO> priorityList = priorityService.queryByOrganizationIdList(organizationId);
                    Map<Long, Object> priorityMap = priorityList.stream().collect(Collectors.toMap(PriorityVO::getId, Function.identity()));
                    setDefaultValueObjsOfMultiple(priorityMap, fieldCascadeRuleVO);
                    break;
                case FieldCode.INFLUENCE_VERSION:
                    List<ProductVersionNameVO> influenceVersionList = modelMapper.map(productVersionMapper.queryNameByOptions(projectId, null), new TypeToken<List<ProductVersionNameVO>>() {
                    }.getType());
                    Map<Long, Object> influenceVersionMap = influenceVersionList.stream().collect(Collectors.toMap(ProductVersionNameVO::getVersionId, Function.identity()));
                    setDefaultValueObjsOfMultiple(influenceVersionMap, fieldCascadeRuleVO);
                    break;
                //多选
                case FieldCode.FIX_VERSION:
                    List<ProductVersionNameVO> fixVersionList = modelMapper.map(productVersionMapper.queryNameByOptions(projectId, Collections.singletonList("version_planning")), new TypeToken<List<ProductVersionNameVO>>() {
                    }.getType());
                    Map<Long, Object> fixVersionMap = fixVersionList.stream().collect(Collectors.toMap(ProductVersionNameVO::getVersionId, Function.identity()));
                    setDefaultValueObjsOfMultiple(fixVersionMap, fieldCascadeRuleVO);
                    break;
                case FieldCode.SUB_PROJECT:
                    if (agilePluginService != null) {
                        List<ProjectRelationshipInfoVO> projectRelationshipInfoList = agilePluginService.getProjUnderGroup(organizationId, projectId, projectId, true);
                        Map<Long, Object> projectMap = projectRelationshipInfoList.stream().collect(Collectors.toMap(ProjectRelationshipInfoVO::getProjectId, Function.identity()));
                        setDefaultValueObjsOfMultiple(projectMap, fieldCascadeRuleVO);
                    }
                    break;
                default:
                    break;
            }
        });
    }

    private void setDefaultUserInfo(FieldCascadeRuleVO fieldCascadeRuleVO, Map<Long, UserMessageDTO> userMap) {
        if (CollectionUtils.isEmpty(fieldCascadeRuleVO.getDefaultIds())) {
            return;
        }
        fieldCascadeRuleVO.setDefaultValueObjs(new ArrayList<>());
        fieldCascadeRuleVO.getDefaultIds().forEach(defaultId -> fieldCascadeRuleVO.getDefaultValueObjs().add(userMap.get(defaultId)));
    }

    private Map<Long, UserMessageDTO> getDefaultUserMap(List<FieldCascadeRuleVO> fieldCascadeRuleList) {
        List<Long> userIds = new ArrayList<>();
        fieldCascadeRuleList.stream().filter(fieldCascadeRuleVO -> {
            List<Long> defaultIds = fieldCascadeRuleVO.getFieldCascadeRuleOptionList()
                    .stream()
                    .filter(fieldCascadeRuleOptionVO -> Boolean.TRUE.equals(fieldCascadeRuleOptionVO.getDefaultOption()))
                    .map(FieldCascadeRuleOptionVO::getCascadeOptionId)
                    .collect(Collectors.toList());
            fieldCascadeRuleVO.setDefaultIds(defaultIds);
            return (FieldType.MEMBER.equals(fieldCascadeRuleVO.getCascadeFieldType()) || FieldType.MULTI_MEMBER.equals(fieldCascadeRuleVO.getCascadeFieldType()))
                    && !CollectionUtils.isEmpty(fieldCascadeRuleVO.getDefaultIds());
        }).forEach(fieldCascadeRuleVO -> userIds.addAll(fieldCascadeRuleVO.getDefaultIds()));
        if (CollectionUtils.isEmpty(userIds)) {
            return new HashMap<>(0);
        }
        return userService.queryUsersMap(userIds, true);
    }

    private void updateAndDeleteFieldCascadeRuleOption(List<FieldCascadeRuleOptionVO> fieldOptionList, Long fieldCascadeRuleId, Long projectId) {
        Long organizationId = ConvertUtil.getOrganizationId(projectId);
        if (CollectionUtils.isEmpty(fieldOptionList)) {
            fieldOptionList = new ArrayList<>();
        }
        List<FieldCascadeRuleOptionDTO> insertOptionList = new ArrayList<>();
        List<FieldCascadeRuleOptionDTO> updateOptionList = new ArrayList<>();
        Map<Long, FieldCascadeRuleOptionDTO> oldOptionIdMap = getRelOptionByRuleId(fieldCascadeRuleId, projectId)
                .stream()
                .collect(Collectors.toMap(FieldCascadeRuleOptionDTO::getCascadeOptionId, Function.identity()));
        FieldCascadeRuleDTO fieldCascadeRuleDTO = fieldCascadeRuleMapper.selectByPrimaryKey(fieldCascadeRuleId);
        ObjectSchemeFieldDTO objectSchemeFieldDTO = objectSchemeFieldMapper.selectByPrimaryKey(fieldCascadeRuleDTO.getCascadeFieldId());
        fieldOptionList.forEach(fieldCascadeRuleOption -> {
            if (oldOptionIdMap.get(fieldCascadeRuleOption.getCascadeOptionId()) != null) {
                FieldCascadeRuleOptionDTO oldOption = oldOptionIdMap.get(fieldCascadeRuleOption.getCascadeOptionId());
                oldOptionIdMap.remove(fieldCascadeRuleOption.getCascadeOptionId());
                Boolean nowOptionDefault = Boolean.TRUE.equals(fieldCascadeRuleOption.getDefaultOption());
                if (!nowOptionDefault.equals(oldOption.getDefaultOption())) {
                    oldOption.setDefaultOption(nowOptionDefault);
                    updateOptionList.add(oldOption);
                }
            } else {
                FieldCascadeRuleOptionDTO fieldCascadeRuleOptionDTO = new FieldCascadeRuleOptionDTO();
                fieldCascadeRuleOptionDTO.setProjectId(projectId);
                fieldCascadeRuleOptionDTO.setOrganizationId(organizationId);
                fieldCascadeRuleOptionDTO.setFieldCascadeRuleId(fieldCascadeRuleId);
                fieldCascadeRuleOptionDTO.setDefaultOption(fieldCascadeRuleOption.getDefaultOption());
                fieldCascadeRuleOptionDTO.setCascadeOptionId(fieldCascadeRuleOption.getCascadeOptionId());
                if (Objects.equals(FieldCode.SUB_PROJECT, objectSchemeFieldDTO.getCode())) {
                    fieldCascadeRuleOptionDTO.setCascadeOptionId(fieldCascadeRuleOption.getProjectId());
                }
                insertOptionList.add(fieldCascadeRuleOptionDTO);
            }
        });

        if (!CollectionUtils.isEmpty(insertOptionList)) {
            insertOptionList.forEach(this::baseCreateFieldCascadeRuleOption);
        }
        if (!CollectionUtils.isEmpty(oldOptionIdMap.keySet())) {
            List<Long> removeIds = oldOptionIdMap.values().stream().map(FieldCascadeRuleOptionDTO::getId).collect(Collectors.toList());
            fieldCascadeRuleOptionMapper.batchDeleteFieldCascadeRuleOptionByIds(removeIds);
        }
        if (!CollectionUtils.isEmpty(updateOptionList)) {
            updateOptionList.forEach(this::baseUpdateFieldCascadeRuleOption);
        }
    }

    private List<FieldCascadeRuleOptionDTO> getRelOptionByRuleId(Long fieldCascadeRuleId, Long projectId) {
        FieldCascadeRuleOptionDTO fieldOptionRecord = new FieldCascadeRuleOptionDTO();
        fieldOptionRecord.setFieldCascadeRuleId(fieldCascadeRuleId);
        fieldOptionRecord.setProjectId(projectId);
        List<FieldCascadeRuleOptionDTO> results = fieldCascadeRuleOptionMapper.select(fieldOptionRecord);
        if (CollectionUtils.isEmpty(results)) {
            results = new ArrayList<>();
        }
        return results;
    }

    private Set<Long> getRelPreviousFieldIds(Long fieldId, Long issueTypeId, Long projectId) {
        Map<Long, List<FieldCascadeRuleDTO>> previousFieldCascadeRuleMap = getPreviousFieldCascadeRuleMap(issueTypeId, projectId);
        Set<Long> previousFieldIds = new HashSet<>();
        if (previousFieldCascadeRuleMap == null || previousFieldCascadeRuleMap.size() == 0) {
            return previousFieldIds;
        }
        addPreviousFieldIds(fieldId, previousFieldIds, previousFieldCascadeRuleMap);
        return previousFieldIds;
    }

    private void validLoop(FieldCascadeRuleDTO fieldCascadeRule) {
        Set<Long> previousFieldIds = getRelPreviousFieldIds(fieldCascadeRule.getFieldId(), fieldCascadeRule.getIssueTypeId(), fieldCascadeRule.getProjectId());
        if (!CollectionUtils.isEmpty(previousFieldIds) && previousFieldIds.contains(fieldCascadeRule.getCascadeFieldId())) {
            throw new CommonException("error.fieldCascadeRule.create.loop");
        }
    }

    private void addPreviousFieldIds(Long fieldId, Set<Long> previousFieldIds, Map<Long, List<FieldCascadeRuleDTO>> previousFieldCascadeRuleMap) {
        List<FieldCascadeRuleDTO> fieldCascadeRuleList = previousFieldCascadeRuleMap.get(fieldId);
        if (CollectionUtils.isEmpty(fieldCascadeRuleList)) {
            return;
        }
        fieldCascadeRuleList.forEach(fieldCascadeRuleDTO -> {
            if (previousFieldIds.contains(fieldCascadeRuleDTO.getFieldId())) {
                return;
            }
            previousFieldIds.add(fieldCascadeRuleDTO.getFieldId());
            addPreviousFieldIds(fieldCascadeRuleDTO.getFieldId(), previousFieldIds, previousFieldCascadeRuleMap);
        });
    }

    private Map<Long, List<FieldCascadeRuleDTO>> getPreviousFieldCascadeRuleMap(Long issueTypeId, Long projectId) {
        FieldCascadeRuleDTO fieldCascadeRuleRecord = new FieldCascadeRuleDTO();
        fieldCascadeRuleRecord.setIssueTypeId(issueTypeId);
        fieldCascadeRuleRecord.setProjectId(projectId);
        List<FieldCascadeRuleDTO> fieldCascadeRuleList = fieldCascadeRuleMapper.select(fieldCascadeRuleRecord);

        if (CollectionUtils.isEmpty(fieldCascadeRuleList)) {
            return null;
        }
        return fieldCascadeRuleList.stream().collect(Collectors.groupingBy(FieldCascadeRuleDTO::getCascadeFieldId));
    }

    private void setDefaultValueObjsOfMultiple(Map<Long, Object> valueMap, FieldCascadeRuleVO fieldCascadeRuleVO) {
        List<Long> defaultIds = fieldCascadeRuleVO.getDefaultIds();
        if (!CollectionUtils.isEmpty(defaultIds)) {
            List<Object> defaultObjs = new ArrayList<>();
            List<Long> newDefaultIds = new ArrayList<>();
            defaultIds.forEach(id -> {
                if (valueMap.containsKey(id)) {
                    defaultObjs.add(valueMap.get(id));
                    newDefaultIds.add(id);
                }
            });
            fieldCascadeRuleVO.setDefaultIds(newDefaultIds);
            fieldCascadeRuleVO.setDefaultValueObjs(defaultObjs);
        }
    }
}
