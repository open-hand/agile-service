package io.choerodon.agile.infra.feign.operator;

import java.util.List;

import com.fasterxml.jackson.core.type.TypeReference;
import io.choerodon.agile.api.vo.AppServiceRepVO;
import io.choerodon.agile.api.vo.AppServiceSimpleVO;
import io.choerodon.agile.api.vo.IssueWithBranchVO;
import io.choerodon.agile.infra.feign.DevopsFeignClient;
import io.choerodon.core.domain.Page;
import org.apache.commons.lang3.BooleanUtils;
import org.hzero.core.util.ResponseUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

/**
 * @author superlee
 * @since 2021-03-12
 */
@Component
public class DevopsClientOperator {
    @Autowired
    private DevopsFeignClient devopsFeignClient;

    public List<AppServiceRepVO> listAppService(Long projectId, int page, int size, Boolean checkMember, Boolean active) {
        return ResponseUtils.getResponse(
                devopsFeignClient.listAppService(projectId, page, size, checkMember, active), new TypeReference<Page<AppServiceRepVO>>() {
        }).getContent();
    }

    public List<AppServiceRepVO> listActiveAppService(Long projectId) {
        return ResponseUtils.getResponse(
                devopsFeignClient.listActiveAppService(projectId),
                new TypeReference<List<AppServiceRepVO>>() {
                }
        );
    }

    public List<IssueWithBranchVO> getIssueIdsBetweenTags(Long projectId, Long appServiceId, String source, String target) {
        return ResponseUtils.getResponse(
                devopsFeignClient.getIssueIdsBetweenTags(projectId, appServiceId, target, source),
                new TypeReference<List<IssueWithBranchVO>>() {
                }
        );
    }

    public List<AppServiceSimpleVO> listByProjectIdAndCode(Long organizationId, List<AppServiceSimpleVO> appServiceList) {
        return ResponseUtils.getResponse(
                devopsFeignClient.listByProjectIdAndCode(organizationId, appServiceList),
                new TypeReference<List<AppServiceSimpleVO>>() {
                }
        );
    }

    public Boolean checkExistIssueBranchRel(Long projectId, Long issueId) {
        Boolean response = ResponseUtils.getResponse(devopsFeignClient.checkIssueBranchRelExist(projectId, issueId), Boolean.class);
        return BooleanUtils.isTrue(response);
    }

    public void copyIssueRelatedBranches(Long projectId, Long oldIssueId, Long newIssueId) {
        devopsFeignClient.copyIssueBranchRel(projectId, oldIssueId, newIssueId);
    }
}
