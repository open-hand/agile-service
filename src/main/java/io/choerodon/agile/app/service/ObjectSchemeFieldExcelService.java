package io.choerodon.agile.app.service;

import javax.servlet.http.HttpServletResponse;

/**
 * @author chihao.ran@hand-china.com
 * 2021/02/01 19:11
 */
public interface ObjectSchemeFieldExcelService {

    /**
     * 下载自定义字段导入模板
     * @param organizationId 组织id
     * @param projectId 项目id
     * @param response response
     */
    void download(Long organizationId, Long projectId, HttpServletResponse response);
}
