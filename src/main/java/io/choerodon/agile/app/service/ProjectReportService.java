package io.choerodon.agile.app.service;

import io.choerodon.agile.api.vo.ProjectReportVO;
import io.choerodon.agile.infra.dto.ProjectReportDTO;
import io.choerodon.core.domain.Page;
import io.choerodon.mybatis.pagehelper.domain.PageRequest;

/**
 * @author jiaxu.cui@hand-china.com 2020/9/15 上午11:08
 */
public interface ProjectReportService {
    Page<ProjectReportDTO> page(ProjectReportVO projectReport, PageRequest pageRequest);

    ProjectReportVO detail(Long projectId, Long id);

    void create(Long projectId, ProjectReportVO projectReportDTO);

    void delete(Long projectId, Long projectReportId);

    void update(Long projectId, ProjectReportVO projectReportVO);
}
