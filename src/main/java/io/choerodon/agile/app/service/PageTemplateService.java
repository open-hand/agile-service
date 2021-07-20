package io.choerodon.agile.app.service;

import java.util.List;

import io.choerodon.agile.api.vo.PageConfigFieldVO;
import io.choerodon.agile.api.vo.PageConfigVO;
import io.choerodon.agile.api.vo.PageTemplateVO;

/**
 * @author chihao.ran@hand-china.com
 * 2021/07/20 16:26
 */
public interface PageTemplateService {

    /**
     * 查询页面模板
     * @param organizationId 组织id
     * @param projectId 项目id
     * @param issueTypeId 问题类型id
     * @return 页面模板
     */
    PageTemplateVO queryPageTemplate(Long organizationId, Long projectId, Long issueTypeId);
}
