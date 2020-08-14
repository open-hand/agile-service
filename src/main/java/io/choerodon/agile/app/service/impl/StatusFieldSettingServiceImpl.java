package io.choerodon.agile.app.service.impl;

import io.choerodon.agile.api.vo.StatusFieldSettingVO;
import io.choerodon.agile.app.service.ProjectConfigService;
import io.choerodon.agile.app.service.StatusFieldSettingService;
import io.choerodon.agile.infra.dto.*;
import io.choerodon.agile.infra.feign.BaseFeignClient;
import io.choerodon.agile.infra.mapper.*;
import io.choerodon.agile.infra.utils.ConvertUtil;
import io.choerodon.core.exception.CommonException;
import org.apache.commons.lang.StringUtils;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;
import org.springframework.util.ObjectUtils;

import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;

/**
 * @author zhaotianxin
 * @date 2020-08-13 14:51
 */
@Service
@Transactional(rollbackFor = Exception.class)
public class StatusFieldSettingServiceImpl implements StatusFieldSettingService {
    private static final String[] FILTER_FIELD_TYPE = {"checkbox", "multiple", "member", "radio", "single"};
    @Autowired
    private StatusFieldSettingMapper statusFieldSettingMapper;
    @Autowired
    private StatusFieldValueSettingMapper statusFieldValueSettingMapper;
    @Autowired
    private ModelMapper modelMapper;
    @Autowired
    private ProjectConfigService projectConfigService;
    @Autowired
    private IssueComponentMapper issueComponentMapper;
    @Autowired
    private IssueLabelMapper issueLabelMapper;
    @Autowired
    private ProductVersionMapper productVersionMapper;
    @Autowired
    private BaseFeignClient baseFeignClient;
    @Autowired
    private PriorityMapper priorityMapper;
    @Autowired
    private FieldOptionMapper fieldOptionMapper;
    @Autowired
    private IssueMapper issueMapper;

    @Override
    public List<StatusFieldSettingVO> createOrUpdate(Long project, Long issueType, Long statusId, Long objectVersionNumber, String applyType, List<StatusFieldSettingVO> list) {
        List<StatusFieldSettingDTO> statusFieldSettingDTOS = listFieldSetting(project, issueType, statusId);
        if (!CollectionUtils.isEmpty(statusFieldSettingDTOS)) {
            deleteStatusFieldSetting(statusFieldSettingDTOS);
        }
        // 遍历
        for (StatusFieldSettingVO statusFieldSettingVO : list) {
            StatusFieldSettingDTO map = modelMapper.map(statusFieldSettingVO, StatusFieldSettingDTO.class);
            map.setProjectId(project);
            map.setStatusId(statusId);
            map.setIssueTypeId(issueType);
            baseInsert(map);
            // 插入field值
            List<StatusFieldValueSettingDTO> fieldValueList = statusFieldSettingVO.getFieldValueList();
            if (!CollectionUtils.isEmpty(fieldValueList)) {
                insertStatusFieldValue(project, map.getId(), fieldValueList);
            }
        }
        // 更新node
        projectConfigService.updateNodeObjectVersionNumber(project, issueType, statusId, objectVersionNumber, applyType);
        return list(project, issueType, statusId);
    }

    @Override
    public List<StatusFieldSettingDTO> listFieldSetting(Long project, Long issueType, Long statusId) {
        StatusFieldSettingDTO statusFieldSettingDTO = new StatusFieldSettingDTO();
        statusFieldSettingDTO.setIssueTypeId(issueType);
        statusFieldSettingDTO.setStatusId(statusId);
        statusFieldSettingDTO.setProjectId(project);
        return statusFieldSettingMapper.select(statusFieldSettingDTO);
    }

    @Override
    public List<StatusFieldSettingVO> list(Long projectId, Long issueType, Long statusId) {
        List<StatusFieldSettingDTO> statusFieldSettingDTOS = listFieldSetting(projectId, issueType, statusId);
        if (CollectionUtils.isEmpty(statusFieldSettingDTOS)) {
            return new ArrayList<>();
        }
        List<StatusFieldSettingVO> list = new ArrayList<>();
        for (StatusFieldSettingDTO statusFieldSettingDTO : statusFieldSettingDTOS) {
            StatusFieldSettingVO map = modelMapper.map(statusFieldSettingDTO, StatusFieldSettingVO.class);
            map.setFieldValueList(listFieldValueSetting(projectId, map.getId()));
            list.add(map);
        }
        return list;
    }

    @Override
    public List<StatusFieldSettingVO> listByStatusIds(Long projectId, Long issueType, List<Long> statusIds) {
        List<StatusFieldSettingVO> list = statusFieldSettingMapper.listByStatusIds(projectId, issueType, statusIds);
        list.forEach(statusFieldSettingVO -> {
            String fieldType = statusFieldSettingVO.getFieldType();
            List<String> fieldTypes = Arrays.asList(FILTER_FIELD_TYPE);
            List<StatusFieldValueSettingDTO> statusFieldValueSettingDTOS = listFieldValueSetting(projectId, statusFieldSettingVO.getId());
            if (!fieldTypes.contains(fieldType)) {
                statusFieldSettingVO.setFieldValueList(statusFieldValueSettingDTOS);
                return;
            }
            if (!Objects.equals("specifier", statusFieldValueSettingDTOS.get(0).getOperateType())) {
                statusFieldSettingVO.setFieldValueList(statusFieldValueSettingDTOS);
                return;
            }
            if ("member".equals(statusFieldSettingVO.getFieldType())) {
                // 查询用户信息
                List<Long> userIds = statusFieldValueSettingDTOS.stream().map(StatusFieldValueSettingDTO::getUserId).collect(Collectors.toList());
                List<UserDTO> body = baseFeignClient.listUsersByIds(userIds.toArray(new Long[userIds.size()]), false).getBody();
                if (CollectionUtils.isEmpty(body)) {
                    statusFieldSettingVO.setFieldValueList(statusFieldValueSettingDTOS);
                    return;
                }
                Map<Long, UserDTO> userDTOMap = body.stream().collect(Collectors.toMap(UserDTO::getId, Function.identity()));
                statusFieldValueSettingDTOS.forEach(v -> v.setName(ObjectUtils.isEmpty(userDTOMap.get(v.getUserId())) ? null : userDTOMap.get(v.getUserId()).getRealName()));
                statusFieldSettingVO.setFieldValueList(statusFieldValueSettingDTOS);
                return;
            }
            handlerFieldValue(statusFieldSettingVO, statusFieldValueSettingDTOS);
            statusFieldSettingVO.setFieldValueList(statusFieldValueSettingDTOS);
        });
        return list;
    }

    private void handlerFieldValue(StatusFieldSettingVO statusFieldSettingVO, List<StatusFieldValueSettingDTO> statusFieldValueSettingDTOS) {
        List<Long> ids = statusFieldValueSettingDTOS.stream().map(StatusFieldValueSettingDTO::getOptionId).collect(Collectors.toList());
        if (Boolean.TRUE.equals(statusFieldSettingVO.getSystem())) {
            switch (statusFieldSettingVO.getFieldCode()) {
                case "component":
                    List<IssueComponentDTO> issueComponentDTOS = issueComponentMapper.selectByIds(StringUtils.join(ids, ","));
                    if (CollectionUtils.isEmpty(issueComponentDTOS)) {
                        break;
                    }
                    Map<Long, IssueComponentDTO> collect = issueComponentDTOS.stream().collect(Collectors.toMap(IssueComponentDTO::getComponentId, Function.identity()));
                    statusFieldValueSettingDTOS.forEach(v -> v.setName(ObjectUtils.isEmpty(collect.get(v.getOptionId())) ? null : collect.get(v.getOptionId()).getName()));
                    break;
                case "label":
                    List<IssueLabelDTO> issueLabelDTOS = issueLabelMapper.selectByIds(StringUtils.join(ids, ","));
                    if (CollectionUtils.isEmpty(issueLabelDTOS)) {
                        break;
                    }
                    Map<Long, IssueLabelDTO> labelDTOMap = issueLabelDTOS.stream().collect(Collectors.toMap(IssueLabelDTO::getLabelId, Function.identity()));
                    statusFieldValueSettingDTOS.forEach(v -> v.setName(ObjectUtils.isEmpty(labelDTOMap.get(v.getOptionId())) ? null : labelDTOMap.get(v.getOptionId()).getLabelName()));
                    break;
                case "influenceVersion":
                case "fixVersion":
                    List<ProductVersionDTO> productVersionDTOS = productVersionMapper.selectByIds(StringUtils.join(ids, ","));
                    if (CollectionUtils.isEmpty(productVersionDTOS)) {
                        break;
                    }
                    Map<Long, ProductVersionDTO> versionDTOMap = productVersionDTOS.stream().collect(Collectors.toMap(ProductVersionDTO::getVersionId, Function.identity()));
                    statusFieldValueSettingDTOS.forEach(v -> v.setName(ObjectUtils.isEmpty(versionDTOMap.get(v.getOptionId())) ? null : versionDTOMap.get(v.getOptionId()).getName()));
                    break;
                case "priority":
                    List<PriorityDTO> priorityDTOS = priorityMapper.selectByIds(StringUtils.join(ids, ","));
                    if (CollectionUtils.isEmpty(priorityDTOS)) {
                        break;
                    }
                    Map<Long, PriorityDTO> priorityDTOMap = priorityDTOS.stream().collect(Collectors.toMap(PriorityDTO::getId, Function.identity()));
                    statusFieldValueSettingDTOS.forEach(v -> v.setName(ObjectUtils.isEmpty(priorityDTOMap.get(v.getOptionId())) ? null : priorityDTOMap.get(v.getOptionId()).getName()));
                    break;
                case "epic":
                    List<IssueDTO> issueDTOS = issueMapper.selectByIds(StringUtils.join(ids, ","));
                    if (CollectionUtils.isEmpty(issueDTOS)) {
                        break;
                    }
                    Map<Long, IssueDTO> issueDTOMap = issueDTOS.stream().collect(Collectors.toMap(IssueDTO::getIssueId, Function.identity()));
                    statusFieldValueSettingDTOS.forEach(v -> v.setName(ObjectUtils.isEmpty(issueDTOMap.get(v.getOptionId())) ? null : issueDTOMap.get(v.getOptionId()).getSummary()));
                    break;
            }
        } else {
            List<FieldOptionDTO> fieldOptionDTOS = fieldOptionMapper.selectByIds(StringUtils.join(ids, ","));
            if (CollectionUtils.isEmpty(fieldOptionDTOS)) {
                return;
            }
            Map<Long, FieldOptionDTO> fieldOptionDTOMap = fieldOptionDTOS.stream().collect(Collectors.toMap(FieldOptionDTO::getId, Function.identity()));
            statusFieldValueSettingDTOS.forEach(v -> v.setName(ObjectUtils.isEmpty(fieldOptionDTOMap.get(v.getOptionId())) ? null : fieldOptionDTOMap.get(v.getOptionId()).getValue()));
        }
    }

    private List<StatusFieldValueSettingDTO> listFieldValueSetting(Long projectId, Long fieldSettingId) {
        StatusFieldValueSettingDTO statusFieldValueSettingDTO = new StatusFieldValueSettingDTO();
        statusFieldValueSettingDTO.setProjectId(projectId);
        statusFieldValueSettingDTO.setStatusFieldSettingId(fieldSettingId);
        return statusFieldValueSettingMapper.select(statusFieldValueSettingDTO);
    }

    private void deleteStatusFieldSetting(List<StatusFieldSettingDTO> statusFieldSettingDTOS) {
        for (StatusFieldSettingDTO statusFieldSettingDTO : statusFieldSettingDTOS) {
            StatusFieldValueSettingDTO statusFieldValueSettingDTO = new StatusFieldValueSettingDTO();
            statusFieldValueSettingDTO.setProjectId(statusFieldSettingDTO.getProjectId());
            statusFieldValueSettingDTO.setStatusFieldSettingId(statusFieldSettingDTO.getId());
            statusFieldValueSettingMapper.delete(statusFieldValueSettingDTO);
            statusFieldSettingMapper.deleteByPrimaryKey(statusFieldSettingDTO.getId());
        }
    }

    private void insertStatusFieldValue(Long projectId, Long fieldSettingId, List<StatusFieldValueSettingDTO> fieldValueList) {
        for (StatusFieldValueSettingDTO statusFieldValueSettingDTO : fieldValueList) {
            statusFieldValueSettingDTO.setStatusFieldSettingId(fieldSettingId);
            statusFieldValueSettingDTO.setProjectId(projectId);
            baseInertFieldValue(statusFieldValueSettingDTO);
        }
    }

    private void baseInertFieldValue(StatusFieldValueSettingDTO statusFieldValueSettingDTO) {
        if (statusFieldValueSettingMapper.insert(statusFieldValueSettingDTO) != 1) {
            throw new CommonException("error.insert.status.field.value.setting");
        }
    }

    private void baseInsert(StatusFieldSettingDTO map) {
        if (statusFieldSettingMapper.insert(map) != 1) {
            throw new CommonException("error.insert.status.field.setting");
        }
    }
}
