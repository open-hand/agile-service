package io.choerodon.agile.app.service.impl;

import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.JSONObject;
import io.choerodon.agile.api.vo.BatchUpdateFieldsValueVo;
import io.choerodon.agile.api.vo.PageFieldViewUpdateVO;
import io.choerodon.agile.app.service.FieldValueService;
import io.choerodon.agile.app.service.IssueFieldValueService;
import io.choerodon.agile.infra.enums.ObjectSchemeCode;
import io.choerodon.agile.infra.feign.NotifyFeignClient;
import io.choerodon.agile.infra.utils.EnumUtil;
import io.choerodon.core.exception.CommonException;
import io.choerodon.core.oauth.DetailsHelper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;
import org.springframework.util.ObjectUtils;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

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
    private NotifyFeignClient notifyFeignClient;
    @Autowired
    private FieldValueService fieldValueService;

    @Async
    @Override
    public void asyncUpdateFields(Long projectId, String schemeCode, BatchUpdateFieldsValueVo batchUpdateFieldsValueVo) {
        Long userId = DetailsHelper.getUserDetails().getUserId();
        Map<String,String> code = new HashMap<>();
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
                fieldValueService.handlerPredefinedFields(projectId, issueIds, predefinedFields);
            }

            // 批量修改issue自定义字段值
            if (!CollectionUtils.isEmpty(batchUpdateFieldsValueVo.getCustomFields())) {
                List<PageFieldViewUpdateVO> customFields = batchUpdateFieldsValueVo.getCustomFields();
                fieldValueService.handlerCustomFields(projectId, customFields, schemeCode, issueIds);
            }
             //发送websocket
            code.put("status","batch_update_success");
        } catch (Exception e) {
            code.put("status","batch_update_failed");
            throw new CommonException(e, e.getMessage());
        }
        finally {
            notifyFeignClient.postWebSocket(WEBSOCKET_BATCH_UPDATE_FIELD, userId.toString(), JSON.toJSONString(code));
        }
    }
}
