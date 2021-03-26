package io.choerodon.agile.app.service;

import io.choerodon.agile.api.vo.PublishVersionVO;
import io.choerodon.agile.api.vo.VersionTreeVO;

import java.util.List;
import java.util.Set;

/**
 * @author superlee
 * @since 2021-03-19
 */
public interface PublishVersionTreeService {

    /**
     * 查询项目下某个项目群版本的版本树
     *
     * @param projectIds
     * @param rootIds
     * @param organizationId
     * @return
     */
    List<VersionTreeVO> tree(Set<Long> projectIds, Long organizationId, Set<Long> rootIds);

    /**
     * 添加子节点
     *
     * @param projectId
     * @param versionTreeVO
     */
    void add(Long projectId, Long organizationId, VersionTreeVO versionTreeVO);

    /**
     * 删除子节点
     *
     * @param projectId
     * @param versionTreeVO
     */
    void delete(Long projectId, Long organizationId, VersionTreeVO versionTreeVO);

    /**
     * 查询项目下可用的app version
     *
     * @param projectId
     * @param organizationId
     * @param rootId
     * @return
     */
    List<PublishVersionVO> availablePublishVersion(Long projectId, Long organizationId, Long rootId);

    /**
     *
     * @param projectId
     * @param organizationId
     * @param rootId
     * @return
     */
    List<PublishVersionVO> directDescendants(Long projectId, Long organizationId, Long rootId);
}
