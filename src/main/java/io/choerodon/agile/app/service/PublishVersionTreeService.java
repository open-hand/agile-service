package io.choerodon.agile.app.service;

import io.choerodon.agile.api.vo.PublishVersionVO;
import io.choerodon.agile.api.vo.TagVO;
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
     * @param projectId
     * @param organizationId
     * @param rootId
     * @return
     */
    List<PublishVersionVO> directDescendants(Long projectId, Long organizationId, Long rootId);

    /**
     * 发布版本树添加tag
     *
     * @param projectId
     * @param organizationId
     * @param publishVersionId
     * @param tags
     */
    void addTag(Long projectId, Long organizationId, Long publishVersionId, Set<TagVO> tags);

    /**
     * 发布版本树删除tag
     *
     * @param projectId
     * @param organizationId
     * @param publishVersionId
     * @param tags
     */
    void deleteTag(Long projectId, Long organizationId, Long publishVersionId, Set<TagVO> tags);

    /**
     * 更新发布版本tag关系的tag别名
     *
     * @param projectId
     * @param tagId
     * @param publishVersionId
     * @param objectVersionNumber
     * @param alias
     */
    void updateTagAlias(Long projectId,
                        Long tagId,
                        Long publishVersionId,
                        Long objectVersionNumber,
                        String alias);
}
