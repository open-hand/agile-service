package io.choerodon.agile.domain.repository;

import io.choerodon.agile.infra.dto.business.IssueDTO;

import java.util.List;
import java.util.Set;

/**
 * @author superlee
 * @since 2022-08-24
 */
public interface IssueRepository {

    /**
     * 根据id集合查询
     *
     * @param ids
     * @return
     */
    List<IssueDTO> selectByIds(Set<Long> ids);

}
