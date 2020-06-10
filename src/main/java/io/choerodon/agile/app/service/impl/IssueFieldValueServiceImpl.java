package io.choerodon.agile.app.service.impl;

import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.JSONObject;
import io.choerodon.agile.api.vo.BatchUpdateFieldStatusVO;
import io.choerodon.agile.api.vo.BatchUpdateFieldsValueVo;
import io.choerodon.agile.api.vo.PageFieldViewUpdateVO;
import io.choerodon.agile.app.service.FieldValueService;
import io.choerodon.agile.app.service.IssueFieldValueService;
import io.choerodon.agile.infra.enums.ObjectSchemeCode;
//import io.choerodon.agile.infra.feign.NotifyFeignClient;
import io.choerodon.agile.infra.utils.EnumUtil;
import io.choerodon.core.exception.CommonException;
import io.choerodon.core.oauth.DetailsHelper;
import org.hzero.boot.message.MessageClient;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;
import org.springframework.util.ObjectUtils;

import java.util.List;

/**
 * @author zhaotianxin
 * @date 2020-05-06 16:08
 */
@Service
public class IssueFieldValueServiceImpl implements IssueFieldValueService {
    private static final String ERROR_SCHEMECODE_ILLEGAL = "error.schemeCode.illegal";
    private static final String WEBSOCKET_BATCH_UPDATE_FIELD = "agile-batch-update-field";
    private static final String ERROR_ISSUE_ID = "error.issueIds.null";

//    @Autowired
//    private NotifyFeignClient notifyFeignClient;
    @Autowired
    private FieldValueService fieldValueService;

    @Autowired
    private MessageClient messageClient;

    @Async
    @Override
    public void asyncUpdateFields(Long projectId, String schemeCode, BatchUpdateFieldsValueVo batchUpdateFieldsValueVo,String applyType) {
        Long userId = DetailsHelper.getUserDetails().getUserId();
        BatchUpdateFieldStatusVO batchUpdateFieldStatusVO = new BatchUpdateFieldStatusVO();
        try {
            batchUpdateFieldStatusVO.setStatus("doing");
            batchUpdateFieldStatusVO.setKey(WEBSOCKET_BATCH_UPDATE_FIELD+"-"+projectId);
            batchUpdateFieldStatusVO.setUserId(userId);
            batchUpdateFieldStatusVO.setProcess(0.0);
//            notifyFeignClient.postWebSocket(WEBSOCKET_BATCH_UPDATE_FIELD, userId.toString(), JSON.toJSONString(batchUpdateFieldStatusVO));
            messageClient.sendByUserId(userId, WEBSOCKET_BATCH_UPDATE_FIELD, JSON.toJSONString(batchUpdateFieldStatusVO));
            if (Boolean.FALSE.equals(EnumUtil.contain(ObjectSchemeCode.class, schemeCode))) {
                throw new CommonException(ERROR_SCHEMECODE_ILLEGAL);
            }
            List<Long> issueIds = batchUpdateFieldsValueVo.getIssueIds();
            if (ObjectUtils.isEmpty(issueIds)) {
                throw new CommonException(ERROR_ISSUE_ID);
            }
            List<PageFieldViewUpdateVO> customFields = batchUpdateFieldsValueVo.getCustomFields();
            int customFieldSize = CollectionUtils.isEmpty(customFields) ? 0 :customFields.size();
            JSONObject predefinedFields = batchUpdateFieldsValueVo.getPredefinedFields();
            int allCount = (ObjectUtils.isEmpty(predefinedFields) ? 0 : issueIds.size()) + customFieldSize;
            double incrementalValue = 1.0 / (allCount == 0 ? 1 : allCount);
            batchUpdateFieldStatusVO.setIncrementalValue(incrementalValue);
            //修改issue预定义字段值
            if (!CollectionUtils.isEmpty(batchUpdateFieldsValueVo.getPredefinedFields())) {
                fieldValueService.handlerPredefinedFields(projectId, issueIds, predefinedFields,batchUpdateFieldStatusVO,applyType);
            }

            // 批量修改issue自定义字段值
            if (!CollectionUtils.isEmpty(customFields)) {
                fieldValueService.handlerCustomFields(projectId, customFields, schemeCode, issueIds,batchUpdateFieldStatusVO);
            }
             //发送websocket
            batchUpdateFieldStatusVO.setStatus("success");
            batchUpdateFieldStatusVO.setProcess(1.0);
        } catch (Exception e) {
            batchUpdateFieldStatusVO.setStatus("failed");
            batchUpdateFieldStatusVO.setError(e.getMessage());
            throw new CommonException("update field failed, exception: {}", e);
        }
        finally {
//            notifyFeignClient.postWebSocket(WEBSOCKET_BATCH_UPDATE_FIELD, userId.toString(), JSON.toJSONString(batchUpdateFieldStatusVO));
            messageClient.sendByUserId(userId, WEBSOCKET_BATCH_UPDATE_FIELD, JSON.toJSONString(batchUpdateFieldStatusVO));
        }
    }
}
