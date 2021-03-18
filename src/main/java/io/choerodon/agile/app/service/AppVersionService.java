package io.choerodon.agile.app.service;

import io.choerodon.agile.api.vo.AppVersionCreateVO;
import io.choerodon.agile.api.vo.AppVersionSearchVO;
import io.choerodon.agile.api.vo.AppVersionUpdateVO;
import io.choerodon.agile.api.vo.AppVersionVO;
import io.choerodon.core.domain.Page;
import io.choerodon.mybatis.pagehelper.domain.PageRequest;

import org.springframework.web.multipart.MultipartFile;

import java.util.List;

/**
 * @author superlee
 * @since 2021-03-10
 */
public interface AppVersionService {

    /**
     * 创建应用版本
     * @param projectId 项目id
     * @param appVersionCreateVO 要创建的应用版本
     * @return 创建的应用版本
     */
    AppVersionVO createAppVersion(Long projectId, AppVersionCreateVO appVersionCreateVO);

    /**
     * 更新应用版本
     * @param projectId 项目id
     * @param appVersionId 要更新的应用版本id
     * @param appVersionUpdateVO 更新的应用版本内容
     * @return 更新的应用版本内容
     */
    AppVersionVO updateAppVersion(Long projectId, Long appVersionId, AppVersionUpdateVO appVersionUpdateVO);


    /**
     * 根据id查询单条应用版本
     * @param projectId 项目id
     * @param appVersionId 应用版本id
     * @return 应用版本
     */
    AppVersionVO queryAppVersionById(Long projectId, Long appVersionId);

    /**
     * 删除应用版本
     * @param projectId 项目id
     * @param appVersionId 应用版本id
     */
    void deleteAppVersion(Long projectId, Long appVersionId);

    /**
     * 是否重复
     *
     * @param projectId 项目id
     * @param appVersionVO 要校验的应用版本tag
     * @return 是否重复
     */
    Boolean checkRepeat(Long projectId, AppVersionVO appVersionVO);

    /**
     * 解析pom文件
     *
     * @param projectId
     * @param groupIds
     * @param multipartFile
     * @return
     */
    List<AppVersionVO> parsePom(Long projectId, String groupIds, MultipartFile multipartFile);

    /**
     * 批量创建应用版本
     * @param projectId 项目id
     * @param appVersionCreateVOList 要创建的应用版本
     * @return 创建的应用版本
     */
    List<AppVersionVO> batchCreateAppVersion(Long projectId, List<AppVersionCreateVO> appVersionCreateVOList);

    /**
     * 删除应用版本和产品版本的关系
     * @param projectId 项目id
     * @param appVersionId 应用版本
     * @param productVersionId 产品版本
     */
    void deleteAppVersionProductVersionRel(Long projectId, Long appVersionId, Long productVersionId);

    /**
     * 删除应用版本和产品版本的关系
     * @param projectId 项目id
     * @param appVersionId 应用版本
     * @param issueId 问题id
     */
    void deleteAppVersionIssueRel(Long projectId, Long appVersionId, Long issueId);

    /**
     * 查询项目下应用版本
     * @param projectId 项目id
     * @param appVersionSearchVO 查询参数
     * @param pageRequest 分页参数
     * @return 项目下应用版本
     */
    Page<AppVersionVO> listAppVersionByProjectId(Long projectId, AppVersionSearchVO appVersionSearchVO, PageRequest pageRequest);

    void updateParentId(List<AppVersionVO> appVersions);
}
