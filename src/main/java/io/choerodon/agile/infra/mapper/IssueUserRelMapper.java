package io.choerodon.agile.infra.mapper;

import io.choerodon.agile.infra.dto.IssueUserRelDTO;
import io.choerodon.mybatis.common.BaseMapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;

/**
 * @author huaxin.deng@hand-china.com
 * @since 2022/4/12
 */
public interface IssueUserRelMapper extends BaseMapper<IssueUserRelDTO> {

    void deleteByIssueIdAndUserIdsWithType(@Param("projectId") Long projectId,
                                           @Param("issueId") Long issueId,
                                           @Param("userIds") List<Long> userIds,
                                           @Param("userType") String userType);

    List<Long> listUserIdsByIssueId(@Param("projectId") Long projectId,
                                    @Param("issueId") Long issueId,
                                    @Param("userType") String userType);
}
