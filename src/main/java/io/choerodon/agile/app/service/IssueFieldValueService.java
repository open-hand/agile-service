package io.choerodon.agile.app.service;

import org.springframework.web.context.request.ServletRequestAttributes;

import io.choerodon.agile.api.vo.BatchUpdateFieldsValueVo;

/**
 * @author zhaotianxin
 * @date 2020-05-06 17:14
 */
public interface IssueFieldValueService {
    /**
     * 批量修改issue的预定义字段和自定义字段的值
     *
     * @param projectId projectId
     * @param schemeCode schemeCode
     * @param batchUpdateFieldsValueVo batchUpdateFieldsValueVo
     */
    void asyncUpdateFields(Long projectId,
                           String schemeCode,
                           BatchUpdateFieldsValueVo batchUpdateFieldsValueVo,
                           String applyType,
                           ServletRequestAttributes requestAttributes,
                           String encryptType);

}
