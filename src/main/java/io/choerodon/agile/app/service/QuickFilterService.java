package io.choerodon.agile.app.service;

import io.choerodon.agile.api.vo.QuickFilterSearchVO;
import io.choerodon.agile.api.vo.QuickFilterVO;
import io.choerodon.agile.api.vo.QuickFilterSequenceVO;
import io.choerodon.core.domain.Page;
import io.choerodon.mybatis.pagehelper.domain.PageRequest;

import java.util.List;


/**
 * Created by HuangFuqiang@choerodon.io on 2018/6/13.
 * Email: fuqianghuang01@gmail.com
 */
public interface QuickFilterService {

    QuickFilterVO create(Long projectId, QuickFilterVO quickFilterVO);

    QuickFilterVO update(Long projectId, Long filterId, QuickFilterVO quickFilterVO);

    void deleteById(Long projectId, Long filterId);

    QuickFilterVO queryById(Long projectId, Long filterId);

    /**
     * 分页搜索过滤条件
     *
     * @param projectId            projectId
     * @param quickFilterSearchVO quickFilterSearchVO
     * @return QuickFilterVO
     */
    Page<QuickFilterVO> listByProjectId(Long projectId, QuickFilterSearchVO quickFilterSearchVO, PageRequest pageRequest);

    /**
     * 拖动排序
     *
     * @param projectId              projectId
     * @param quickFilterSequenceVO quickFilterSequenceVO
     * @return QuickFilterVO
     */
    QuickFilterVO dragFilter(Long projectId, QuickFilterSequenceVO quickFilterSequenceVO);

    Boolean checkName(Long projectId, String quickFilterName);

    String inSql(String operation,String value);
}
