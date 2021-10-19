package io.choerodon.agile.app.service;

import io.choerodon.agile.api.vo.WorkHoursSearchVO;
import org.springframework.web.context.request.ServletRequestAttributes;

import java.util.List;

/**
 * @author zhaotianxin
 * @date 2021-10-19 14:11
 */
public interface WorkHoursExcelService {
    /**
     * 导出工时日志
     * @param organizationId
     * @param projectIds
     * @param workHoursSearchVO
     * @param requestAttributes
     */
    void exportWorkHoursLog(Long organizationId, List<Long> projectIds, WorkHoursSearchVO workHoursSearchVO, ServletRequestAttributes requestAttributes, Boolean isOrg);
}
