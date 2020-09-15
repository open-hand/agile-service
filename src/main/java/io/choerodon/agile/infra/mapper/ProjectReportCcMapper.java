package io.choerodon.agile.infra.mapper;

import java.util.List;

import io.choerodon.agile.api.vo.ProjectReportVO;
import io.choerodon.agile.infra.dto.ProjectReportCcDTO;
import io.choerodon.mybatis.common.BaseMapper;

/**
 * @author jiaxu.cui@hand-china.com 2020/9/15 下午5:54
 */
public interface ProjectReportCcMapper extends BaseMapper<ProjectReportCcDTO> {


    List<ProjectReportVO> List(ProjectReportVO projectReport);
}
