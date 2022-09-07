package io.choerodon.agile.infra.feign.operator;

import com.fasterxml.jackson.core.type.TypeReference;
import io.choerodon.agile.api.vo.AppServiceRepVO;
import io.choerodon.agile.api.vo.AppServiceSimpleVO;
import io.choerodon.agile.api.vo.IssueWithBranchVO;
import io.choerodon.agile.infra.feign.DevopsFeignClient;
import io.choerodon.core.domain.Page;
import io.choerodon.core.exception.ServiceUnavailableException;
import io.choerodon.core.utils.FeignClientUtils;
import org.hzero.core.util.ResponseUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.List;

/**
 * @author superlee
 * @since 2021-03-12
 */
@Component
public class DevopsClientOperator {
    @Autowired
    private DevopsFeignClient devopsFeignClient;

    public List<AppServiceRepVO> listAppService(Long projectId, int page, int size, Boolean checkMember, Boolean active) {
        try {
            return FeignClientUtils.doRequest(() ->
                    devopsFeignClient.listAppService(projectId, page, size, checkMember, active), new TypeReference<Page<AppServiceRepVO>>() {
            }).getContent();
        } catch (ServiceUnavailableException e) {
            return new ArrayList<>();
        }
    }

    public List<AppServiceRepVO> listActiveAppService(Long projectId) {
        try {
            return
                    ResponseUtils.getResponse(
                            devopsFeignClient.listActiveAppService(projectId),
                            new TypeReference<List<AppServiceRepVO>>() {
                            }
                    );
        } catch (ServiceUnavailableException e) {
            return new ArrayList<>();
        }
    }

    public List<IssueWithBranchVO> getIssueIdsBetweenTags(Long projectId, Long appServiceId, String source, String target) {
        try {
            return
                    ResponseUtils.getResponse(
                            devopsFeignClient.getIssueIdsBetweenTags(projectId, appServiceId, target, source),
                            new TypeReference<List<IssueWithBranchVO>>() {
                            }
                    );
        } catch (ServiceUnavailableException e) {
            return new ArrayList<>();
        }
    }

    public List<AppServiceSimpleVO> listByProjectIdAndCode(Long organizationId, List<AppServiceSimpleVO> appServiceList) {
        try {
            return
                    ResponseUtils.getResponse(
                            devopsFeignClient.listByProjectIdAndCode(organizationId, appServiceList),
                            new TypeReference<List<AppServiceSimpleVO>>() {
                            }
                    );
        } catch (ServiceUnavailableException e) {
            return new ArrayList<>();
        }
    }

    public boolean checkExistIssueBranchRel(Long projectId, Long issueId) {
        try {
            return devopsFeignClient.checkIssueBranchRelExist(projectId, issueId).getBody();
        } catch (ServiceUnavailableException e) {
            return false;
        }
    }

    public void copyIssueRelatedBranches(Long projectId, Long oldIssueId, Long newIssueId) {
        try{
            devopsFeignClient.copyIssueBranchRel(projectId, oldIssueId, newIssueId).getBody();
        } catch (ServiceUnavailableException e) {
            return;
        }
    }
}
