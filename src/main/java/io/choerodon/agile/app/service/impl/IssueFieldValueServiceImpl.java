package io.choerodon.agile.app.service.impl;

import com.alibaba.fastjson.JSONObject;
import io.choerodon.agile.api.vo.*;
import io.choerodon.agile.app.service.IssueFieldValueService;
import io.choerodon.agile.app.service.IssueService;
import io.choerodon.agile.app.service.ObjectSchemeFieldService;
import io.choerodon.agile.infra.dto.FieldValueDTO;
import io.choerodon.agile.infra.dto.IssueDTO;
import io.choerodon.agile.infra.dto.ObjectSchemeFieldDTO;
import io.choerodon.agile.infra.enums.FieldType;
import io.choerodon.agile.infra.enums.ObjectSchemeCode;
import io.choerodon.agile.infra.feign.NotifyFeignClient;
import io.choerodon.agile.infra.mapper.FieldDataLogMapper;
import io.choerodon.agile.infra.mapper.FieldValueMapper;
import io.choerodon.agile.infra.mapper.IssueMapper;
import io.choerodon.agile.infra.utils.EnumUtil;
import io.choerodon.agile.infra.utils.FieldValueUtil;
import io.choerodon.agile.infra.utils.StringUtil;
import io.choerodon.agile.infra.utils.VerifyUpdateUtil;
import io.choerodon.core.exception.CommonException;
import io.choerodon.core.oauth.CustomUserDetails;
import io.choerodon.core.oauth.DetailsHelper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;
import org.springframework.util.ObjectUtils;

import java.util.*;
import java.util.stream.Collectors;

/**
 * @author zhaotianxin
 * @date 2020-05-06 16:08
 */
@Service
public class IssueFieldValueServiceImpl implements IssueFieldValueService {
    private static final String ERROR_SCHEMECODE_ILLEGAL = "error.schemeCode.illegal";
    private static final String ERROR_FIELDTYPE_ILLEGAL = "error.fieldType.illegal";
    private static final String WEBSOCKET_BATCH_UPDATE_FIELD = "agile-batch-update-field";
    private static final String ERROR_ISSUE_ID = "error.issueIds.null";

    @Autowired
    private FieldValueMapper fieldValueMapper;
    @Autowired
    private ObjectSchemeFieldService objectSchemeFieldService;
    @Autowired
    private IssueMapper issueMapper;
    @Autowired
    private FieldDataLogMapper fieldDataLogMapper;
    @Autowired
    private VerifyUpdateUtil verifyUpdateUtil;
    @Autowired
    private IssueService issueService;
    @Autowired
    private NotifyFeignClient notifyFeignClient;

    @Async
    @Override
    public void asyncUpdateFields(Long projectId, String schemeCode, BatchUpdateFieldsValueVo batchUpdateFieldsValueVo) {
        Long userId = DetailsHelper.getUserDetails().getUserId();
        try {
            if (Boolean.FALSE.equals(EnumUtil.contain(ObjectSchemeCode.class, schemeCode))) {
                throw new CommonException(ERROR_SCHEMECODE_ILLEGAL);
            }
            List<Long> issueIds = batchUpdateFieldsValueVo.getIssueIds();
            if (ObjectUtils.isEmpty(issueIds)) {
                throw new CommonException(ERROR_ISSUE_ID);
            }
            // 修改issue预定义字段值
            if (!CollectionUtils.isEmpty(batchUpdateFieldsValueVo.getPredefinedFields())) {
                JSONObject predefinedFields = batchUpdateFieldsValueVo.getPredefinedFields();
                handlerPredefinedFields(projectId, issueIds, predefinedFields);
            }

            // 批量修改issue自定义字段值
            if (!CollectionUtils.isEmpty(batchUpdateFieldsValueVo.getCustomFields())) {
                List<PageFieldViewUpdateVO> customFields = batchUpdateFieldsValueVo.getCustomFields();
                handlerCustomFields(projectId, customFields, schemeCode, issueIds);
            }
            // 发送websocket
            notifyFeignClient.postWebSocket(WEBSOCKET_BATCH_UPDATE_FIELD, userId.toString(), "batch_update_success");
        } catch (Exception e) {
            notifyFeignClient.postWebSocket(WEBSOCKET_BATCH_UPDATE_FIELD, userId.toString(), "batch_update_failed");
            throw new CommonException(e, e.getMessage());
        }
    }

    @Override
    public void handlerPredefinedFields(Long projectId, List<Long> issueIds, JSONObject predefinedFields) {
        List<IssueDTO> issueDTOS = issueMapper.listIssueInfoByIssueIds(projectId, issueIds);
        if (CollectionUtils.isEmpty(issueDTOS)) {
            throw new CommonException("error.issues.null");
        }
        List<VersionIssueRelVO> fixVersion = StringUtil.cast(predefinedFields.get("fixVersion"));
        List<VersionIssueRelVO> influenceVersion = StringUtil.cast(predefinedFields.get("influenceVersion"));
        predefinedFields.remove("fixVersion");
        predefinedFields.remove("influenceVersion");
        issueDTOS.forEach(v -> {
            IssueUpdateVO issueUpdateVO = new IssueUpdateVO();
            List<String> fieldList = verifyUpdateUtil.verifyUpdateData(predefinedFields, issueUpdateVO);
            if (!"story".equals(v.getTypeCode())) {
                fieldList.remove(String.valueOf("storyPoints"));
                issueUpdateVO.setStoryPoints(null);
            }

            if ("issue_epic".equals(v.getTypeCode())) {
                fieldList.remove(String.valueOf("epicId"));
                issueUpdateVO.setEpicId(null);
            }

            if (!CollectionUtils.isEmpty(fixVersion)) {
                issueUpdateVO.setVersionType("fix");
                issueUpdateVO.setVersionIssueRelVOList(fixVersion);
            }

            issueUpdateVO.setIssueId(v.getIssueId());
            issueUpdateVO.setObjectVersionNumber(v.getObjectVersionNumber());
            IssueVO issueVO = issueService.updateIssue(projectId, issueUpdateVO, fieldList);
            if ("bug".equals(v.getTypeCode()) && !ObjectUtils.isEmpty(v.getRelateIssueId())) {
                IssueUpdateVO issueUpdateVO1 = new IssueUpdateVO();
                if (!CollectionUtils.isEmpty(influenceVersion)) {
                    issueUpdateVO1.setVersionType("influence");
                    issueUpdateVO1.setVersionIssueRelVOList(influenceVersion);
                }
                issueUpdateVO1.setIssueId(v.getIssueId());
                issueUpdateVO1.setObjectVersionNumber(issueVO.getObjectVersionNumber());
                issueService.updateIssue(projectId, issueUpdateVO1, new ArrayList<>());
            }

        });
    }
    @Override
    public void handlerCustomFields(Long projectId, List<PageFieldViewUpdateVO> customFields, String schemeCode, List<Long> issueIds) {
        List<IssueDTO> issueDTOS = issueMapper.listIssueInfoByIssueIds(projectId, issueIds);
        if (CollectionUtils.isEmpty(customFields)) {
            throw new CommonException("error.customFields.null");
        }
        // 判断这个字段哪些问题类型可以添加
        customFields.forEach(v -> {
            ObjectSchemeFieldDTO objectSchemeFieldDTO = objectSchemeFieldService.selectById(v.getFieldId());
            String context = objectSchemeFieldDTO.getContext();
            if ("global".equals(context)) {
                batchHandlerCustomFields(projectId, v, schemeCode, issueIds);
            } else {
                String[] split = context.split(",");
                if (ObjectUtils.isEmpty(split)) {
                    throw new CommonException("error.context.null");
                }
                List<String> contexts = Arrays.asList(split);
                List<Long> needAddIssueIds = issueDTOS.stream().filter(issueDTO -> contexts.contains(issueDTO.getTypeCode())).map(IssueDTO::getIssueId).collect(Collectors.toList());
                if (CollectionUtils.isEmpty(needAddIssueIds)) {
                    throw new CommonException(ERROR_ISSUE_ID);
                }
                batchHandlerCustomFields(projectId, v, schemeCode, needAddIssueIds);
            }
        });
    }

    protected void batchHandlerCustomFields(Long projectId, PageFieldViewUpdateVO pageFieldViewUpdateVO, String schemeCode, List<Long> needAddIssueIds) {
        if (Boolean.FALSE.equals(EnumUtil.contain(FieldType.class, pageFieldViewUpdateVO.getFieldType()))) {
            throw new CommonException(ERROR_FIELDTYPE_ILLEGAL);
        }
        Long fieldId = pageFieldViewUpdateVO.getFieldId();
        //获取原fieldValue
        List<FieldValueDTO> oldFieldValues = fieldValueMapper.listByInstanceIdsAndFieldId(projectId, needAddIssueIds, schemeCode, fieldId);
        //删除原fieldValue
        Map<Long, List<FieldValueDTO>> oldFieldMap = new HashMap<>();
        if (!oldFieldValues.isEmpty()) {
            fieldValueMapper.deleteByInstanceIds(projectId, needAddIssueIds, schemeCode, fieldId);
            oldFieldMap.putAll(oldFieldValues.stream().collect(Collectors.groupingBy(FieldValueDTO::getInstanceId)));
        }
        List<FieldValueDTO> allFieldValue = new ArrayList<>();
        needAddIssueIds.forEach(issueId -> {
            //创建新fieldValue
            List<FieldValueDTO> newFieldValues = new ArrayList<>();
            FieldValueUtil.handleValue2DTO(newFieldValues, pageFieldViewUpdateVO.getFieldType(), pageFieldViewUpdateVO.getValue());
            newFieldValues.forEach(fieldValue -> {
                fieldValue.setFieldId(fieldId);
                fieldValue.setInstanceId(issueId);
                fieldValue.setFieldType(pageFieldViewUpdateVO.getFieldType());
            });
            allFieldValue.addAll(newFieldValues);
        });
        // 批量写入表中
        fieldValueMapper.batchInsertField(projectId, schemeCode, allFieldValue);
        // 批量生产日志
        Map<Long, List<FieldValueDTO>> newFieldMap = new HashMap<>();
        newFieldMap.putAll(allFieldValue.stream().collect(Collectors.groupingBy(FieldValueDTO::getInstanceId)));
        List<FieldDataLogCreateVO> list = new ArrayList<>();
        needAddIssueIds.forEach(v -> {
            List<FieldValueDTO> oldFiledList = oldFieldMap.get(v);
            List<FieldValueDTO> newFiledList = newFieldMap.get(v);
            list.addAll(FieldValueUtil.batchHandlerFiledLog(projectId, v, oldFiledList, newFiledList));
        });
        if (!CollectionUtils.isEmpty(list) && "agile_issue".equals(schemeCode)) {
            CustomUserDetails customUserDetails = DetailsHelper.getUserDetails();
            fieldDataLogMapper.batchInsert(projectId, schemeCode, list, customUserDetails.getUserId());
        }
    }
}
