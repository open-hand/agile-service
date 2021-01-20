package io.choerodon.agile.app.service;

import io.choerodon.agile.infra.dto.StaticFileHeaderDTO;
import io.choerodon.agile.infra.dto.StaticFileIssueRelDTO;

/**
 * @author chihao.ran@hand-china.com
 * 2021/01/11 11:27
 */
public interface StaticFileDealService {

    /**
     * 创建静态文件头记录
     * @param staticFileHeaderDTO 静态文件头记录
     * @param staticFileIssueRelDTO 关联关系
     * @return 静态文件头记录
     */
    StaticFileHeaderDTO createBase(StaticFileHeaderDTO staticFileHeaderDTO, StaticFileIssueRelDTO staticFileIssueRelDTO);

    /**
     * 删除静态文件与问题的关联记录
     * @param staticFileHeaderDTO 静态文件
     * @param staticFileIssueRelDTO 关联关系
     */
    void deleteStaticFileRelated(StaticFileHeaderDTO staticFileHeaderDTO, StaticFileIssueRelDTO staticFileIssueRelDTO);

    /**
     * 创建静态文件与问题的关联关系
     * @param newRelDTO 要创建的关联关系
     * @param staticFileHeaderDTO 静态文件头
     */
    void updateStaticFileRelatedIssue(StaticFileIssueRelDTO newRelDTO, StaticFileHeaderDTO staticFileHeaderDTO);

    /**
     * 删除静态文件所有的关联关系
     * @param relRecord 查询条件
     * @param staticFileHeader 要删除的静态文件
     */
    void deleteRel(StaticFileIssueRelDTO relRecord, StaticFileHeaderDTO staticFileHeader);
}
