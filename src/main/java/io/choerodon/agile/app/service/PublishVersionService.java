package io.choerodon.agile.app.service;

import io.choerodon.agile.api.vo.*;
import io.choerodon.agile.api.vo.business.IssueListFieldKVVO;
import io.choerodon.agile.infra.dto.TagCompareHistoryDTO;
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
     * @param projectId          项目id
     * @param publishVersionId
     * @param publishVersionList 要创建的发布版本
     * @return 创建的发布版本
     */
    List<PublishVersionVO> batchCreate(Long projectId, Long publishVersionId, List<PublishVersionVO> publishVersionList);

    /**
     * 查询项目下发布版本
     *
     * @param projectId        项目id
     * @param publishVersionVO 查询参数
     * @param pageRequest      分页参数
     * @return 项目下发布版本
     */
    Page<PublishVersionVO> list(Long projectId, PublishVersionVO publishVersionVO, PageRequest pageRequest);

    /**
     * 别名重名校验
     *
     * @param projectId
     * @param alias
     * @param publishVersionId
     * @return
     */
    Boolean checkAlias(Long projectId, String alias, Long publishVersionId);

    /**
     * 查询版本关联的故事
     *
     * @param projectId
     * @param publishVersionId
     * @param searchVO
     * @return
     */
    Page<IssueListFieldKVVO> listRelIssueByOption(Long projectId,
                                                  Long organizationId,
                                                  Long publishVersionId,
                                                  SearchVO searchVO,
                                                  PageRequest pageRequest);

    void compareTag(Long projectId,
                    Long organizationId,
                    Long publishVersionId,
                    List<TagCompareVO> tagCompareList,
                    String action);

    /**
     * 删除tag相关数据
     *
     * @param projectId
     * @param appServiceCode
     * @param tagName
     */
    void deleteTag(Long projectId, String appServiceCode, String tagName);

    /**
     * 更新发布版本状态
     *
     * @param projectId
     * @param publishVersionId
     * @param statusCode
     * @param objectVersionNumber
     */
    void updateStatus(Long projectId, Long publishVersionId, String statusCode, Long objectVersionNumber);

    /**
     * 查询tag 对比历史
     *
     * @param projectId
     * @param organizationId
     * @param publishVersionId
     * @return
     */
    List<TagCompareHistoryDTO> tagCompareHistory(Long projectId, Long organizationId, Long publishVersionId);

    /**
     * 查看tag对比的issue
     *
     * @param projectId
     * @param organizationId
     * @param publishVersionId
     * @param tagCompareVO
     * @return
     */
    List<IssueListFieldKVVO> previewIssueFromTag(Long projectId,
                                                 Long organizationId,
                                                 Long publishVersionId,
                                                 TagCompareVO tagCompareVO);

    /**
     * 查询发布版本可用的appService
     *
     * @param projectId
     * @param publishVersionId
     * @return
     */
    List<AppServiceRepVO> activeAppService(Long projectId, Long publishVersionId);

    /**
     * 发布版本关联的tag关联的问题类型统计
     *
     * @param projectId
     * @param publishVersionId
     * @return
     */
    List<IssueTypeCountVO> issueTypeCount(Long projectId, Long publishVersionId);
}
