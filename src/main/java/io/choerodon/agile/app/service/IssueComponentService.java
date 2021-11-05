package io.choerodon.agile.app.service;

import io.choerodon.agile.api.vo.ComponentForListVO;
import io.choerodon.agile.api.vo.IssueComponentVO;
import io.choerodon.agile.api.vo.business.IssueVO;
import io.choerodon.agile.api.vo.SearchVO;
import io.choerodon.agile.infra.dto.ComponentForListDTO;
import io.choerodon.core.domain.Page;
import io.choerodon.agile.infra.dto.IssueComponentDTO;
import io.choerodon.mybatis.pagehelper.domain.PageRequest;

import java.util.List;

/**
 * Created by HuangFuqiang@choerodon.io on 2018/5/14.
 * Email: fuqianghuang01@gmail.com
 */

public interface IssueComponentService {

    IssueComponentVO create(Long projectId, IssueComponentVO issueComponentVO);

    IssueComponentVO update(Long projectId, Long id, IssueComponentVO issueComponentVO, List<String> fieldList);

    void delete(Long projectId, Long id, Long relateComponentId);

    IssueComponentVO queryComponentsById(Long projectId, Long id);

    Page<ComponentForListVO> queryComponentByProjectId(Long projectId, Long componentId, Boolean noIssueTest, SearchVO searchVO, PageRequest pageRequest);

    List<IssueVO> queryIssuesByComponentId(Long projectId, Long componentId);

    List<ComponentForListVO> listByProjectIdForTest(Long projectId, Long componentId, Boolean noIssueTest);

    Boolean checkComponentName(Long projectId, String componentName);

    IssueComponentDTO createBase(IssueComponentDTO issueComponentDTO);

    IssueComponentDTO updateBase(IssueComponentDTO issueComponentDTO);

    void deleteBase(Long id);

    Page<ComponentForListDTO> pagedQueryComponentsByOptions(List<Long> projectIds,
                                                            Long ignoredComponentId,
                                                            Boolean noIssueTest,
                                                            SearchVO searchVO,
                                                            PageRequest pageRequest);
}
