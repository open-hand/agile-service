package io.choerodon.agile.infra.mapper;

import java.util.List;
import java.util.Set;

import io.choerodon.agile.api.vo.IssueLabelVO;
import io.choerodon.agile.api.vo.LabelIssueRelVO;
import io.choerodon.mybatis.common.BaseMapper;
import io.choerodon.agile.infra.dto.*;
import org.apache.ibatis.annotations.Param;


/**
 * 敏捷开发Issue标签
 *
 * @author dinghuang123@gmail.com
 * @since 2018-05-14 21:04:00
 */
public interface IssueLabelMapper extends BaseMapper<IssueLabelDTO> {

    /**
     * 回收没有再用的issue标签
     *
     * @param projectId projectId
     * @return int
     */
    int labelGarbageCollection(@Param("projectId") Long projectId);

    /**
     * 重名校验
     *
     * @param labelName labelName
     * @param projectId projectId
     * @return 重名true 否 false
     */
    Boolean checkNameExist(@Param("labelName") String labelName, @Param("projectId") Long projectId);

    /**
     * 根据label名称和项目id查询labelId
     *
     * @param labelName labelName
     * @param projectId projectId
     * @return Long
     */
    Long queryLabelIdByLabelNameAndProjectId(@Param("labelName") String labelName, @Param("projectId") Long projectId);

    /**
     * 根据项目id集合查询标签
     *
     * @param projectIds
     * @return
     */
    List<IssueLabelDTO> selectByProjectIds(@Param("projectIds") List<Long> projectIds);

    /**
     * 根据issueIds 查询标签
     * @param projectIds
     * @param issueIds
     * @return
     */
    List<LabelIssueRelVO> listByIssueIds(@Param("projectIds") Set<Long> projectIds, @Param("issueIds") List<Long> issueIds);

    /**
     * 查询项目下的标签
     * @param projectId
     * @return
     */
    List<IssueLabelVO> listByProjectId(@Param("projectId") Long projectId);
}