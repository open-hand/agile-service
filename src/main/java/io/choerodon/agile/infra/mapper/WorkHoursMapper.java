package io.choerodon.agile.infra.mapper;

import io.choerodon.agile.api.vo.WorkHoursLogVO;
import io.choerodon.agile.api.vo.WorkHoursSearchVO;
import org.apache.ibatis.annotations.Param;

import java.util.List;

/**
 * @author zhaotianxin
 * @date 2021-10-18 10:16
 */
public interface WorkHoursMapper {

    List<WorkHoursLogVO> listByProjectIds(@Param("projectIds") List<Long> projectIds, @Param("workHoursSearchVO") WorkHoursSearchVO workHoursSearchVO);

}
