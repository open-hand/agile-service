package io.choerodon.agile.app.service;

import org.apache.poi.ss.usermodel.Workbook;
import org.springframework.web.context.request.RequestAttributes;

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

    /**
     * 导入自定义字段
     * @param organizationId 组织id
     * @param projectId 项目id
     * @param workbook 上传的excel
     * @param requestAttributes requestAttributes
     */
    void batchImport(Long organizationId, Long projectId, Workbook workbook, RequestAttributes requestAttributes);
}
