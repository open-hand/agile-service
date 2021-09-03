package io.choerodon.agile.infra.mapper;

import io.choerodon.agile.infra.dto.IssueSprintRelDTO;
import io.choerodon.mybatis.common.BaseMapper;

import org.apache.ibatis.annotations.Param;

import java.util.List;

/**
 * @author dinghuang123@gmail.com
 * @since 2018/7/6
 */
public interface IssueSprintRelMapper extends BaseMapper<IssueSprintRelDTO> {

    IssueSprintRelDTO selectNoClosed(@Param("projectId") Long projectId, @Param("issueId") Long issueId);

    /**
     * 查询对应的所有冲刺与问题的关系id
     * @param projectId 项目id
     * @param issueIds 问题id
     * @return 所有冲刺与问题的关系id
     */
    List<Long> selectAllIssueSprintRelIds(@Param("projectId") Long projectId, @Param("issueIds") List<Long> issueIds);

    /**
     * 删除冲刺与问题关系
     * @param issueSprintRelIds 关系id
     * @return 影响数量
     */
    int deleteByIds(@Param("issueSprintRelIds") List<Long> issueSprintRelIds);
}
