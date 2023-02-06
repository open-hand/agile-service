package io.choerodon.agile.domain.repository;

import java.util.List;

import io.choerodon.agile.api.vo.IssuePersonalSortVO;
import io.choerodon.agile.infra.dto.IssuePersonalSortDTO;

import org.hzero.mybatis.base.BaseRepository;

/**
 * 用户个性化排序规则 Repository
 * @author gaokuo.dai@zknow.com 2023-02-02
 */
public interface PersonalSortRepository extends BaseRepository<IssuePersonalSortDTO> {

    /**
     * 根据业务类型查询用户个性化排序规则
     * @param organizationId    组织ID
     * @param projectId         项目ID, 不传默认为0
     * @param businessType      业务类型
     * @return                  查询结果
     */
    List<IssuePersonalSortVO> listByBusinessType(Long organizationId, Long projectId, String businessType);

}
