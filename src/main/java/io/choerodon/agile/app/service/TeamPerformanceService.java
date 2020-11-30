package io.choerodon.agile.app.service;

import io.choerodon.agile.api.vo.SprintStoryPointVO;
import io.choerodon.agile.api.vo.SprintTaskVO;

import java.util.List;

public interface TeamPerformanceService {
    /**
     * 查询当前项目正在进行冲刺各人员故事点统计数据
     * @param projectId
     * @return
     */
    List<SprintStoryPointVO> querySprintStoryPoint(Long projectId);

    /**
     * 查询当前项目正在进行冲刺任务工时统计数据
     * @param projectId
     * @return
     */
    List<SprintTaskVO> querySprintTaskTime(Long projectId);
}
