package io.choerodon.agile.infra.mapper;

import io.choerodon.agile.infra.dto.ColumnStatusRelDTO;
import io.choerodon.mybatis.common.BaseMapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;

/**
 * Created by HuangFuqiang@choerodon.io on 2018/5/15.
 * Email: fuqianghuang01@gmail.com
 */
public interface ColumnStatusRelMapper extends BaseMapper<ColumnStatusRelDTO> {

    /**
     * 根据statusTo为null的issueId集合和列id集合查询columnStatus关系，用于统计累积流图
     *
     * @param statusToNullIssueIds statusToNullIssueIds
     * @param columnIds            columnIds
     * @return ColumnStatusRelDTO
     */
    List<ColumnStatusRelDTO> queryByIssueIdAndColumnIds(@Param("statusToNullIssueIds") List<Long> statusToNullIssueIds, @Param("columnIds") List<Long> columnIds);

    List<ColumnStatusRelDTO> selectStatusRel(@Param("organizationId") Long organizationId, @Param("projectId") Long projectId,@Param("columnId") Long columnId, @Param("statusId") Long statusId);

    int updatePosition(@Param("columnStatusRel") ColumnStatusRelDTO columnStatusRelDTO);

    List<Long> queryStatusIds(@Param("organizationId") Long organizationId, @Param("projectId") Long projectId, @Param("boardTemplateId") Long boardTemplateId);
}
