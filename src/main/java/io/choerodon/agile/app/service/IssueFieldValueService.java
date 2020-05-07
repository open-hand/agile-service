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

}
