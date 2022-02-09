package io.choerodon.agile.app.service.impl;

import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.JSONObject;
import io.choerodon.agile.api.vo.BatchUpdateFieldStatusVO;
import io.choerodon.agile.api.vo.BatchUpdateFieldsValueVo;
import io.choerodon.agile.api.vo.PageFieldViewUpdateVO;
import io.choerodon.agile.api.vo.business.TriggerCarrierVO;
import io.choerodon.agile.app.service.FieldValueService;
import io.choerodon.agile.app.service.IssueFieldValueService;
import io.choerodon.agile.app.service.IssueService;
import io.choerodon.agile.infra.enums.ObjectSchemeCode;
import io.choerodon.agile.infra.mapper.ObjectSchemeFieldMapper;
import io.choerodon.agile.infra.utils.EnumUtil;
import io.choerodon.core.client.MessageClientC7n;
import io.choerodon.core.exception.CommonException;
import io.choerodon.core.oauth.DetailsHelper;
import org.hzero.starter.keyencrypt.core.EncryptContext;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;
import org.springframework.util.ObjectUtils;
import org.springframework.web.context.request.RequestContextHolder;
import org.springframework.web.context.request.ServletRequestAttributes;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * @author zhaotianxin
 * @date 2020-05-06 16:08
 */
@Service
public class IssueFieldValueServiceImpl implements IssueFieldValueService {
    private static final String ERROR_SCHEMECODE_ILLEGAL = "error.schemeCode.illegal";
    private static final String WEBSOCKET_BATCH_UPDATE_FIELD = "agile-batch-update-field";
    private static final String ERROR_ISSUE_ID = "error.issueIds.null";

    @Autowired
    private FieldValueService fieldValueService;

    @Autowired
    private MessageClientC7n messageClientC7n;

    @Autowired
    private ObjectSchemeFieldMapper objectSchemeFieldMapper;

    @Autowired
    private IssueService issueService;

    @Async
    @Override
    public void asyncUpdateFields(Long projectId,
                                  String schemeCode,
                                  BatchUpdateFieldsValueVo batchUpdateFieldsValueVo,
                                  String applyType,
                                  ServletRequestAttributes requestAttributes,
                                  String encryptType) {
        EncryptContext.setEncryptType(encryptType);
        RequestContextHolder.setRequestAttributes(requestAttributes);
        Long userId = DetailsHelper.getUserDetails().getUserId();
        String messageCode = WEBSOCKET_BATCH_UPDATE_FIELD+"-"+projectId;
        BatchUpdateFieldStatusVO batchUpdateFieldStatusVO = new BatchUpdateFieldStatusVO();
        try {
            batchUpdateFieldStatusVO.setLastProcess(0.0);
            batchUpdateFieldStatusVO.setStatus("doing");
            batchUpdateFieldStatusVO.setKey(messageCode);
            batchUpdateFieldStatusVO.setUserId(userId);
            batchUpdateFieldStatusVO.setProcess(0.0);
            messageClientC7n.sendByUserId(userId, messageCode, JSON.toJSONString(batchUpdateFieldStatusVO));
            if (Boolean.FALSE.equals(EnumUtil.contain(ObjectSchemeCode.class, schemeCode))) {
                throw new CommonException(ERROR_SCHEMECODE_ILLEGAL);
            }
            List<Long> issueIds = batchUpdateFieldsValueVo.getIssueIds();
            if (ObjectUtils.isEmpty(issueIds)) {
                throw new CommonException(ERROR_ISSUE_ID);
            }
            List<PageFieldViewUpdateVO> customFields = batchUpdateFieldsValueVo.getCustomFields();
            // 过滤掉不存在的字段
            customFields = filterNotExistFields(customFields);
            JSONObject predefinedFields = batchUpdateFieldsValueVo.getPredefinedFields();
            int allCount = (ObjectUtils.isEmpty(predefinedFields) ? 0 : issueIds.size());
            double incrementalValue = 1.0 / (allCount == 0 ? 1 : allCount);
            batchUpdateFieldStatusVO.setIncrementalValue(incrementalValue);
            Map<Long, TriggerCarrierVO> triggerCarrierMap = new HashMap<>();
            fieldValueService.handlerIssueFields(projectId, issueIds, predefinedFields,batchUpdateFieldStatusVO,applyType, true, schemeCode, customFields, triggerCarrierMap);
            issueService.batchUpdateInvokeTrigger(triggerCarrierMap.values().stream().collect(Collectors.toList()));
             //发送websocket
            batchUpdateFieldStatusVO.setStatus("success");
            batchUpdateFieldStatusVO.setProcess(1.0);
        } catch (Exception e) {
            batchUpdateFieldStatusVO.setStatus("failed");
            batchUpdateFieldStatusVO.setError(e.getMessage());
            throw new CommonException("update field failed, exception: {}", e);
        }
        finally {
            messageClientC7n.sendByUserId(userId, messageCode, JSON.toJSONString(batchUpdateFieldStatusVO));
        }
    }

    private List<PageFieldViewUpdateVO> filterNotExistFields(List<PageFieldViewUpdateVO> customFields) {
        if (CollectionUtils.isEmpty(customFields)) {
            return new ArrayList<>();
        }
        List<Long> fieldIds = customFields.stream().map(PageFieldViewUpdateVO::getFieldId).collect(Collectors.toList());
        List<Long> existFields = objectSchemeFieldMapper.filterNotExistFields(fieldIds);
        if (!CollectionUtils.isEmpty(existFields)) {
            List<PageFieldViewUpdateVO> pageFieldViewUpdateVOS = customFields.stream().filter(customField -> existFields.contains(customField.getFieldId())).collect(Collectors.toList());
            return CollectionUtils.isEmpty(pageFieldViewUpdateVOS) ? new ArrayList<>() : pageFieldViewUpdateVOS;
        }
        else {
            return new ArrayList<>();
        }
    }
}
