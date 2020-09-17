package io.choerodon.agile.infra.mapper;

import java.util.List;

import io.choerodon.agile.api.vo.ProjectReportVO;
import io.choerodon.agile.infra.dto.ProjectReportDTO;
import io.choerodon.mybatis.common.BaseMapper;

/**
 * @author jiaxu.cui@hand-china.com 2020/9/15 下午5:54
 */
public interface ProjectReportMapper extends BaseMapper<ProjectReportDTO> {


    List<ProjectReportVO> list(ProjectReportVO projectReport);
}
