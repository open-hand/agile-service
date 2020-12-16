package io.choerodon.agile.app.service;

import io.choerodon.agile.api.vo.SprintBugVO;
import io.choerodon.agile.api.vo.SprintStoryPointVO;
import io.choerodon.agile.api.vo.SprintTaskVO;
import io.choerodon.core.domain.Page;
import io.choerodon.mybatis.pagehelper.domain.PageRequest;

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

    /**
     * 查询当前进行冲刺bug排名
     * @param projectId
     * @param environment
     * @param type
     * @param pageRequest
     * @return
     */
    Page<SprintBugVO> querySprintBugRank(Long projectId, String environment, String type, PageRequest pageRequest);

    /**
     * 查询当前进行冲刺bug数量统计数据
     * @param projectId
     * @param environment
     * @param type
     * @return
     */
    List<SprintBugVO> querySprintBugCount(Long projectId, String environment, String type);

    /**
     * 查询所有冲刺故事点
     * @param projectId
     * @return
     */
    List<SprintStoryPointVO> queryHistorySprintStoryPoint(Long projectId);

    /**
     * 查询所有冲刺任务工时统计
     * @param projectId
     * @return
     */
    List<SprintTaskVO> queryHistorySprintTaskTime(Long projectId);

    /**
     * 所有冲刺bug变化统计
     * @param projectId
     * @param environment
     * @param type
     * @param other
     * @param responsibleIds
     * @return
     */
    List<SprintBugVO> queryHistorySprintBugCount(Long projectId, String environment, String type, Boolean other, List<Long> responsibleIds);

}
