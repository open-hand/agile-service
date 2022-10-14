package io.choerodon.agile.infra.mapper;

import java.util.List;

import org.apache.ibatis.annotations.Param;

import io.choerodon.agile.api.vo.SprintBugVO;
import io.choerodon.agile.api.vo.SprintStoryPointVO;
import io.choerodon.agile.api.vo.SprintTaskVO;
import io.choerodon.agile.infra.dto.SprintDTO;
import io.choerodon.mybatis.common.BaseMapper;

public interface TeamPerformanceMapper extends BaseMapper<SprintDTO> {
    /**
     * 查询当前项目正在进行冲刺各人员故事点统计数据
     *
     * @param projectId projectId
     * @return result
     */
    List<SprintStoryPointVO> querySprintStoryPoints(@Param("projectId") Long projectId);

    /**
     * 查询当前项目正在进行冲刺任务工时统计数据
     *
     * @param projectId projectId
     * @return result
     */
    List<SprintTaskVO> querySprintTaskTime(@Param("projectId") Long projectId);

    /**
     * 查询当前进行冲刺bug排名
     *
     * @param projectId projectId
     * @param environment environment
     * @param type type
     * @return result
     */
    List<SprintBugVO> querySprintBugCount(@Param("projectId") Long projectId,
                                          @Param("environment") String environment,
                                          @Param("type") String type);

    /**
     * 查询所有冲刺故事点
     *
     * @param projectId projectId
     * @return result
     */
    List<SprintStoryPointVO> queryHistorySprintStoryPoint(@Param("projectId") Long projectId);

    /**
     * 查询所有冲刺任务工时统计
     *
     * @param projectId projectId
     * @return result
     */
    List<SprintTaskVO> queryHistorySprintTaskTime(@Param("projectId") Long projectId);

    /**
     * 所有冲刺bug变化统计
     * @param projectId projectId
     * @param environment environment
     * @param type type
     * @param other other
     * @param responsibleIds responsibleIds
     * @return result
     */
    List<SprintBugVO> queryHistorySprintBugCount(@Param("projectId") Long projectId,
                                                 @Param("environment") String environment,
                                                 @Param("type") String type,
                                                 @Param("other") Boolean other,
                                                 @Param("responsibleIds") List<Long> responsibleIds);

    /**
     * 查询项目下所有冲刺负责人
     * @param projectId projectId
     * @return result
     */
    List<Long> queryResponsible(@Param("projectId") Long projectId);
}
