package io.choerodon.agile.app.service.impl;

import io.choerodon.agile.app.service.ProjectReportService;
import io.choerodon.agile.infra.dto.ProjectReportDTO;
import io.choerodon.core.domain.Page;
import io.choerodon.mybatis.pagehelper.domain.PageRequest;

/**
 * @author jiaxu.cui@hand-china.com 2020/9/15 上午11:08
 */
public class ProjectReportServiceImpl implements ProjectReportService {

    @Override
    public Page<ProjectReportDTO> page(Long projectId, PageRequest pageRequest) {
        return null;
    }

    @Override
    public ProjectReportDTO detail(Long projectId, Long id) {
        return null;
    }

    @Override
    public void create(Long projectId, ProjectReportDTO projectReportDTO) {

    }

    @Override
    public void delete(Long projectId, ProjectReportDTO projectReportDTO) {

    }
}
