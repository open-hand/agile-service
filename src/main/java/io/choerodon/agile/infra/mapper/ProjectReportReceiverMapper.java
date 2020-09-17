package io.choerodon.agile.infra.mapper;


import java.util.List;

import io.choerodon.agile.infra.dto.ProjectReportReceiverDTO;
import io.choerodon.mybatis.common.BaseMapper;
import org.apache.ibatis.annotations.Param;

/**
 * @author jiaxu.cui@hand-china.com 2020/9/15 下午5:54
 */
public interface ProjectReportReceiverMapper extends BaseMapper<ProjectReportReceiverDTO> {

    List<ProjectReportReceiverDTO> selectReceiver(@Param("reportIdList") List<Long> reportIdList, 
                                                  @Param("type") String type);
}
