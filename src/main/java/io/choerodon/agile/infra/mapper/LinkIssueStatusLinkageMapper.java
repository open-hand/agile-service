package io.choerodon.agile.infra.mapper;

import io.choerodon.agile.api.vo.StatusLinkageVO;
import io.choerodon.agile.infra.dto.LinkIssueStatusLinkageDTO;
import io.choerodon.mybatis.common.BaseMapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;

/**
 * @author zhaotianxin
 * @date 2021-06-09 11:03
 */
public interface LinkIssueStatusLinkageMapper extends BaseMapper<LinkIssueStatusLinkageDTO> {
    List<StatusLinkageVO> selectWithStatusByProjectId(@Param("projectId") Long projectId);
}
