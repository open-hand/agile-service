package io.choerodon.agile.app.service;

import io.choerodon.agile.api.vo.PublishVersionVO;
import io.choerodon.core.domain.Page;
import io.choerodon.mybatis.pagehelper.domain.PageRequest;

import org.springframework.web.multipart.MultipartFile;

import java.util.List;

/**
 * @author superlee
 * @since 2021-03-10
 */
public interface PublishVersionService {

    /**
     * 创建发布版本
     *
     * @param projectId        项目id
     * @param publishVersionVO 要创建的发布版本
     * @return 创建的发布版本
     */
    PublishVersionVO create(Long projectId, PublishVersionVO publishVersionVO);

    /**
     * 更新发布版本
     *
     * @param projectId        项目id
     * @param publishVersionId 要更新的发布版本id
     * @param publishVersionVO 更新的发布版本内容
     * @return 更新的发布版本内容
     */
    PublishVersionVO update(Long projectId, Long publishVersionId, PublishVersionVO publishVersionVO);


    /**
     * 根据id查询单条发布版本
     *
     * @param projectId        项目id
     * @param publishVersionId 发布版本id
     * @return 发布版本
     */
    PublishVersionVO query(Long projectId, Long publishVersionId);

    /**
     * 删除发布版本
     *
     * @param projectId        项目id
     * @param publishVersionId 发布版本id
     */
    void delete(Long projectId, Long publishVersionId);

    /**
     * 是否重复
     *
     * @param projectId        项目id
     * @param publishVersionVO 要校验的发布版本tag
     * @return 是否重复
     */
    Boolean isExisted(Long projectId, PublishVersionVO publishVersionVO);

    /**
     * 解析pom文件
     *
     * @param projectId
     * @param groupIds
     * @param multipartFile
     * @return
     */
    List<PublishVersionVO> parsePom(Long projectId,
                                    String groupIds,
                                    MultipartFile multipartFile,
                                    Long publishVersionId,
                                    Boolean writeBack);

    /**
     * 批量创建发布版本
     *
     * @param projectId              项目id
     * @param publishVersionList 要创建的发布版本
     * @return 创建的发布版本
     */
    List<PublishVersionVO> batchCreate(Long projectId, List<PublishVersionVO> publishVersionList);

    /**
     * 查询项目下发布版本
     *
     * @param projectId          项目id
     * @param publishVersionVO 查询参数
     * @param pageRequest        分页参数
     * @return 项目下发布版本
     */
    Page<PublishVersionVO> list(Long projectId, PublishVersionVO publishVersionVO, PageRequest pageRequest);
}
