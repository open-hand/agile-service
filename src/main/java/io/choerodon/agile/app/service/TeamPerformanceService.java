package io.choerodon.agile.app.service;

import java.util.List;

import io.choerodon.agile.api.vo.SprintBugVO;
import io.choerodon.agile.api.vo.SprintStoryPointVO;
import io.choerodon.agile.api.vo.SprintTaskVO;
import io.choerodon.agile.infra.dto.UserDTO;
import io.choerodon.core.domain.Page;
import io.choerodon.mybatis.pagehelper.domain.PageRequest;

public interface TeamPerformanceService {
    /**
     * 查询当前项目正在进行冲刺各人员故事点统计数据
     * @param projectId projectId
     * @return result
     */
    List<SprintStoryPointVO> querySprintStoryPoint(Long projectId);

    /**
     * 查询当前项目正在进行冲刺任务工时统计数据
     * @param projectId projectId
     * @return result
     */
    List<SprintTaskVO> querySprintTaskTime(Long projectId);

    /**
     * 查询当前进行冲刺bug排名
     * @param projectId projectId
     * @param environment environment
     * @param type type
     * @param pageRequest pageRequest
     * @return result
     */
    Page<SprintBugVO> querySprintBugRank(Long projectId, String environment, String type, PageRequest pageRequest);

    /**
     * 查询当前进行冲刺bug数量统计数据
     * @param projectId projectId
     * @param environment environment
     * @param type type
     * @return result
     */
    List<SprintBugVO> querySprintBugCount(Long projectId, String environment, String type);

    /**
     * 查询所有冲刺故事点
     * @param projectId projectId
     * @return result
     */
    List<SprintStoryPointVO> queryHistorySprintStoryPoint(Long projectId);

    /**
     * 查询所有冲刺任务工时统计
     * @param projectId projectId
     * @return result
     */
    List<SprintTaskVO> queryHistorySprintTaskTime(Long projectId);

    /**
     * 所有冲刺bug变化统计
     * @param projectId projectId
     * @param environment environment
     * @param type type
     * @param other other
     * @param responsibleIds responsibleIds
     * @return result
     */
    List<SprintBugVO> queryHistorySprintBugCount(Long projectId, String environment, String type, Boolean other, List<Long> responsibleIds);

    /**
     * 查询当前项目下参与冲刺的所有人
     * @param projectId projectId
     * @return result
     */
    List<UserDTO> queryResponsible(Long projectId);
}
