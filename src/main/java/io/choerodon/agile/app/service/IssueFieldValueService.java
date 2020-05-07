package io.choerodon.agile.app.service;

import com.alibaba.fastjson.JSONObject;
import io.choerodon.agile.api.vo.BatchUpdateFieldsValueVo;
import io.choerodon.agile.api.vo.PageFieldViewUpdateVO;

import java.util.List;

/**
 * @author zhaotianxin
 * @date 2020-05-06 17:14
 */
public interface IssueFieldValueService {
    /**
     * 批量修改issue的预定义字段和自定义字段的值
     *
     * @param projectId
     * @param schemeCode
     * @param batchUpdateFieldsValueVo
     */
    void asyncUpdateFields(Long projectId, String schemeCode, BatchUpdateFieldsValueVo batchUpdateFieldsValueVo);

    /**
     * 批量处理自定义字段的值
     * @param projectId
     * @param customFields
     * @param schemeCode
     * @param issueIds
     */
    void handlerCustomFields(Long projectId, List<PageFieldViewUpdateVO> customFields, String schemeCode, List<Long> issueIds);

    /**
     * 批量处理预定义字段值
     * @param projectId
     * @param issueIds
     * @param predefinedFields
     */
    void handlerPredefinedFields(Long projectId, List<Long> issueIds, JSONObject predefinedFields);
}
