package io.choerodon.agile.app.assembler;

import io.choerodon.agile.api.vo.StarBeaconVO;
import io.choerodon.agile.infra.dto.StarBeaconDTO;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.util.Assert;


/**
 * Created by jian_zhang02@163.com on 2018/5/17.
 */
@Component
public class StarBeaconAssembler extends AbstractAssembler{

    private static final String ERROR_PROJECT_IS_NULL = "error.project.is.null";
    private static final String ERROR_ISSUE_PROJECT_IS_NULL = "error.issue.project.is.null";
    private static final String ERROR_ORGANIZATION_IS_NULL = "error.organization.is.null";
    private static final String ERROR_TYPE_IS_NULL = "error.type.is.null";
    private static final String ERROR_INSTANCE_IS_NULL = "error.instance.is.null";

    @Autowired
    private ModelMapper modelMapper;

    public StarBeaconDTO checkAndSet(StarBeaconVO starBeaconVO) {
        Assert.notNull(starBeaconVO.getType(), ERROR_TYPE_IS_NULL);
        Assert.notNull(starBeaconVO.getInstanceId(), ERROR_INSTANCE_IS_NULL);
        Assert.notNull(starBeaconVO.getProjectId(), ERROR_PROJECT_IS_NULL);
        Assert.notNull(starBeaconVO.getIssueProjectId(), ERROR_ISSUE_PROJECT_IS_NULL);
        Assert.notNull(starBeaconVO.getOrganizationId(), ERROR_ORGANIZATION_IS_NULL);
        StarBeaconDTO starBeaconDTO = new StarBeaconDTO();
        modelMapper.map(starBeaconVO, starBeaconDTO);
        return starBeaconDTO;
    }
}
