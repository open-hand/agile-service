package io.choerodon.agile.infra.mapper;

import org.apache.ibatis.annotations.Param;

import java.util.List;

import io.choerodon.agile.infra.dto.StaticFileHeaderDTO;
import io.choerodon.mybatis.common.BaseMapper;

/**
 * @author chihao.ran@hand-china.com
 * 2021/01/13 16:07
 */
public interface StaticFileHeaderMapper extends BaseMapper<StaticFileHeaderDTO> {

    /**
     * 获取该项目下没有关联传入id的所有的静态文件列表
     *
     * @param projectId 项目id
     * @param issueId   问题id
     * @return 静态文件头
     */
    List<StaticFileHeaderDTO> selectFileListExcludeIssue(@Param("projectId") Long projectId, @Param("issueId") Long issueId);

    /**
     * 获取该项目下没有关联的所有的静态文件列表
     *
     * @param projectId 项目id
     * @param issueId   问题id
     * @return 静态文件头
     */
    List<StaticFileHeaderDTO> selectFileListByIssue(@Param("projectId") Long projectId, @Param("issueId") Long issueId);

    /**
     * 更新静态文件状态
     *
     * @param id     静态文件头id
     * @param status 状态
     */
    void updateFileStatus(@Param("id") Long id, @Param("status") String status);

    /**
     * 获取静态文件关联的问题id
     *
     * @param staticFileId 静态文件id
     * @param projectId 项目id
     * @return 关联的问题id
     */
    List<Long> selectRelatedIssueId(@Param("staticFileId") Long staticFileId, @Param("projectId") Long projectId);
}
