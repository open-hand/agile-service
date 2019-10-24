package io.choerodon.agile.infra.mapper;

import io.choerodon.agile.infra.dto.IssueSprintRelDTO;
import io.choerodon.mybatis.common.Mapper;
import org.apache.ibatis.annotations.Param;

/**
 * @author dinghuang123@gmail.com
 * @since 2018/7/6
 */
public interface IssueSprintRelMapper extends Mapper<IssueSprintRelDTO> {

    IssueSprintRelDTO selectNoClosed(@Param("projectId") Long projectId, @Param("issueId") Long issueId);

}
