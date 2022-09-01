package io.choerodon.agile.domain.repository;

import java.util.List;
import java.util.Set;

import io.choerodon.agile.infra.dto.business.IssueDTO;

/**
 * @author superlee
 * @since 2022-08-24
 */
public interface IssueRepository {

    /**
     * 根据id集合查询
     *
     * @param ids ids
     * @return result
     */
    List<IssueDTO> selectByIds(Set<Long> ids);

}
