package io.choerodon.agile.infra.mapper;

import io.choerodon.agile.api.vo.SprintStoryPointVO;
import io.choerodon.agile.api.vo.SprintTaskVO;
import io.choerodon.agile.infra.dto.SprintDTO;
import io.choerodon.mybatis.common.BaseMapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;

public interface TeamPerformanceMapper extends BaseMapper<SprintDTO> {
    /**
     * 查询当前项目正在进行冲刺各人员故事点统计数据
     * @param projectId
     * @param isPlugin
     * @return
     */
    List<SprintStoryPointVO> querySprintStoryPoints(@Param("projectId") Long projectId, @Param("isPlugin") boolean isPlugin);

    /**
     * 查询当前项目正在进行冲刺任务工时统计数据
     * @param projectId
     * @param isPlugin
     * @return
     */
    List<SprintTaskVO> querySprintTaskTime(@Param("projectId") Long projectId, @Param("isPlugin") boolean isPlugin);
}
