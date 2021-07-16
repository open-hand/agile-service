package io.choerodon.agile.app.service.impl;

import org.apache.commons.collections4.CollectionUtils;
import org.modelmapper.ModelMapper;
import org.modelmapper.TypeToken;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import javax.annotation.Resource;

import io.choerodon.agile.api.vo.*;
import io.choerodon.agile.app.service.FieldCascadeRuleService;
import io.choerodon.agile.app.service.ObjectSchemeFieldService;
import io.choerodon.agile.app.service.PriorityService;
import io.choerodon.agile.app.service.UserService;
import io.choerodon.agile.infra.dto.FieldCascadeRuleDTO;
import io.choerodon.agile.infra.dto.FieldCascadeRuleOptionDTO;
import io.choerodon.agile.infra.dto.UserMessageDTO;
import io.choerodon.agile.infra.enums.FieldCode;
import io.choerodon.agile.infra.enums.FieldType;
import io.choerodon.agile.infra.mapper.FieldCascadeRuleMapper;
import io.choerodon.agile.infra.mapper.FieldCascadeRuleOptionMapper;
import io.choerodon.agile.infra.mapper.IssueComponentMapper;
import io.choerodon.agile.infra.utils.ConvertUtil;
import io.choerodon.core.exception.CommonException;

/**
 * @author chihao.ran@hand-china.com
 * 2021/07/13 16:06
 */
@Service
@Transactional(rollbackFor = Exception.class)
public class FieldCascadeRuleServiceImpl implements FieldCascadeRuleService {

    @Resource
    private FieldCascadeRuleMapper fieldCascadeRuleMapper;
    @Resource
    private FieldCascadeRuleOptionMapper fieldCascadeRuleOptionMapper;
    @Resource
    private IssueComponentMapper issueComponentMapper;
    @Autowired
    private PriorityService priorityService;
    @Autowired
    private ModelMapper modelMapper;
    @Autowired
    private ObjectSchemeFieldService objectSchemeFieldService;
    @Autowired
    private UserService userService;

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
            FieldCode.FIX_VERSION,
            FieldCode.INFLUENCE_VERSION,
            FieldCode.PROGRAM_VERSION,
            FieldCode.EPIC,
            FieldCode.SPRINT,
            FieldCode.LABEL,
            FieldCode.FEATURE,
            FieldCode.PI
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
            fieldCascadeCreate.getFieldCascadeRuleOptionList().forEach(fieldCascadeRuleOptionVO -> {
                FieldCascadeRuleOptionDTO fieldCascadeRuleOptionDTO = modelMapper.map(fieldCascadeRuleOptionVO, FieldCascadeRuleOptionDTO.class);
                fieldCascadeRuleOptionDTO.setFieldCascadeRuleId(fieldCascadeRule.getId());
                fieldCascadeRuleOptionDTO.setProjectId(projectId);
                fieldCascadeRuleOptionDTO.setOrganizationId(organizationId);
                baseCreateFieldCascadeRuleOption(fieldCascadeRuleOptionDTO);
            });
        }
        return modelMapper.map(fieldCascadeRule, FieldCascadeRuleVO.class);
    }

    private void validCreateField(FieldCascadeCreateVO fieldCascadeCreate) {
        if (fieldCascadeCreate.getIssueTypeId() == null){
            throw new CommonException("error.fieldCascadeCreate.issueType.null");
        }
        if (fieldCascadeCreate.getFieldId() == null){
            throw new CommonException("error.fieldCascadeCreate.fieldId.null");
        }
        if (fieldCascadeCreate.getCascadeFieldId() == null){
            throw new CommonException("error.fieldCascadeCreate.cascadeFieldId.null");
        }
        if (fieldCascadeCreate.getFieldOptionId() == null){
            throw new CommonException("error.fieldCascadeCreate.fieldOption.null");
        }
    }

    private void validExit(FieldCascadeCreateVO fieldCascadeCreate, Long projectId) {
        FieldCascadeRuleDTO fieldCascadeRuleRecord = new FieldCascadeRuleDTO();
        fieldCascadeRuleRecord.setFieldId(fieldCascadeCreate.getFieldId());
        fieldCascadeRuleRecord.setFieldOptionId(fieldCascadeCreate.getFieldOptionId());
        fieldCascadeRuleRecord.setCascadeFieldId(fieldCascadeCreate.getCascadeFieldId());
        fieldCascadeRuleRecord.setProjectId(projectId);
        if (fieldCascadeRuleMapper.selectCount(fieldCascadeRuleRecord) > 0){
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
        List<FieldCascadeRuleVO> result = fieldCascadeRuleMapper.listFieldCascadeRuleByIssueType(projectId, issueTypeId, fieldId);
        processDefaultValue(projectId, projectId, result);
        return result;
    }

    @Override
    public FieldCascadeRuleVO updateFieldCascadeRule(Long projectId, Long fieldCascadeRuleId, FieldCascadeUpdateVO fieldCascadeUpdate) {
        FieldCascadeRuleDTO fieldCascadeRuleDTO = modelMapper.map(fieldCascadeUpdate, FieldCascadeRuleDTO.class);
        fieldCascadeRuleDTO.setProjectId(projectId);
        fieldCascadeRuleDTO.setId(fieldCascadeRuleId);
        if (fieldCascadeRuleMapper.updateByPrimaryKeySelective(fieldCascadeRuleDTO) != 1) {
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
            fieldCascadeRuleVO.setDefaultIds(fieldCascadeRuleOptionList.stream().map(FieldCascadeRuleOptionDTO::getCascadeOptionId).collect(Collectors.toList()));
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
        if (CollectionUtils.isEmpty(fieldCascadeRuleList)){
            return new ArrayList<>();
        }

        List<FieldCascadeCreateVO> createList = new ArrayList<>();
        List<FieldCascadeRuleVO> updateList = new ArrayList<>();
        List<Long> deleteIdList = new ArrayList<>();

        fieldCascadeRuleList.forEach(fieldCascadeRule -> {
               if (fieldCascadeRule.get_status() == null){
                   return;
               }
               switch (fieldCascadeRule.get_status()){
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
                   default:break;
               }
        });

        batchDeleteFieldCascadeRule(deleteIdList, projectId);
        batchUpdateFieldCascadeRule(updateList, projectId);
        batchCreateFieldCascadeRule(createList, projectId);
        return fieldCascadeRuleList;
    }

    private void batchDeleteFieldCascadeRule(List<Long> deleteIdList, Long projectId) {
        if (CollectionUtils.isEmpty(deleteIdList)) {
            return;
        }
        fieldCascadeRuleOptionMapper.batchDeleteByFieldCascadeRuleIds(deleteIdList, projectId);
        fieldCascadeRuleMapper.batchDeleteByIds(deleteIdList, projectId);
    }

    private void batchUpdateFieldCascadeRule(List<FieldCascadeRuleVO> updateList, Long projectId) {
        if (!CollectionUtils.isEmpty(updateList)){
            updateList.forEach(updateFieldCascadeRule -> {
                FieldCascadeUpdateVO fieldCascadeUpdateVO = modelMapper.map(updateFieldCascadeRule, FieldCascadeUpdateVO.class);
                updateFieldCascadeRule(projectId, updateFieldCascadeRule.getId(), fieldCascadeUpdateVO);
            });
        }
    }

    private void batchCreateFieldCascadeRule(List<FieldCascadeCreateVO> createList, Long projectId) {
        if (!CollectionUtils.isEmpty(createList)){
            createList.forEach(fieldCascadeCreateVO -> createFieldCascadeRule(projectId, fieldCascadeCreateVO));
        }
    }

    private void processDefaultValue(Long projectId, Long organizationId, List<FieldCascadeRuleVO> fieldCascadeRuleList) {
        if (CollectionUtils.isEmpty(fieldCascadeRuleList)) {
            return;
        }
        Map<Long, UserMessageDTO> userMap = getDefaultUserMap(fieldCascadeRuleList);
        fieldCascadeRuleList.forEach(fieldCascadeRuleVO -> {
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
        fieldCascadeRuleList.stream().filter(fieldCascadeRuleVO ->
                (fieldCascadeRuleVO.getCascadeFieldType().equals(FieldType.MEMBER) || fieldCascadeRuleVO.getCascadeFieldType().equals(FieldType.MULTI_MEMBER))
                        && !CollectionUtils.isEmpty(fieldCascadeRuleVO.getDefaultIds()))
                .forEach(fieldCascadeRuleVO -> userIds.addAll(fieldCascadeRuleVO.getDefaultIds()));
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
                fieldCascadeRuleOptionDTO.setFieldCascadeRuleId(fieldCascadeRuleOption.getFieldCascadeRuleId());
                fieldCascadeRuleOptionDTO.setCascadeOptionId(fieldCascadeRuleOption.getCascadeOptionId());
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
