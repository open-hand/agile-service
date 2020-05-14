package io.choerodon.agile.infra.mapper;

import io.choerodon.mybatis.common.BaseMapper;
import io.choerodon.agile.infra.dto.*;
import org.apache.ibatis.annotations.Param;

import java.util.List;


/**
 * @author dinghuang123@gmail.com
 * @since 2018-05-15 16:47:27
 */
public interface ComponentIssueRelMapper extends BaseMapper<ComponentIssueRelDTO> {

    List<ComponentIssueRelDTO> selectByProjectIdAndIssueId(@Param("projectId") Long projectId, @Param("issueId") Long issueId);
}