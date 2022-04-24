package io.choerodon.agile.app.service.impl;

import java.util.Objects;

import io.choerodon.agile.api.vo.StarBeaconVO;
import io.choerodon.agile.app.assembler.StarBeaconAssembler;
import io.choerodon.agile.app.service.BacklogExpandService;
import io.choerodon.agile.infra.dto.StarBeaconDTO;
import io.choerodon.agile.infra.dto.business.IssueDTO;
import io.choerodon.agile.infra.mapper.IssueMapper;
import io.choerodon.agile.infra.mapper.StarBeaconMapper;
import io.choerodon.agile.app.service.StarBeaconService;
import io.choerodon.core.exception.CommonException;
import io.choerodon.core.oauth.DetailsHelper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.Assert;

/**
 * 服务实现类
 *
 * @author jiaxu.cui@hand-china.com
 */
@Service
@Transactional(rollbackFor = Exception.class)
public class StarBeaconServiceImpl implements StarBeaconService {

    @Autowired
    private StarBeaconMapper starBeaconMapper;

    @Autowired
    private IssueMapper issueMapper;
    @Autowired(required = false)
    private BacklogExpandService backlogExpandService;
    @Autowired
    private StarBeaconAssembler starBeaconAssembler;

    private static final String ERROR_NOT_LOGIN = "error.not.login";

    private static final String ERROR_INSTANCE_IS_NULL = "error.instance.is.null";
    private static final String ERROR_STAR_INSTANCE_FAILED = "error.star.instance.failed";
    private static final String ERROR_DELETE_STAR_INSTANCE_FAILED = "error.delete.star.instance.failed";
    private static final String STAR_BEACON_TYPE_ISSUE = "issue";

    @Override
    public void starInstance(StarBeaconVO starBeaconVO) {
        StarBeaconDTO starBeaconDTO = starBeaconAssembler.checkAndSet(starBeaconVO);
        Long userId = DetailsHelper.getUserDetails().getUserId();
        Assert.notNull(userId, ERROR_NOT_LOGIN);
        starBeaconDTO.setUserId(userId);
        if (starBeaconVO.getType().equals(STAR_BEACON_TYPE_ISSUE)) {
            IssueDTO issueDTO = new IssueDTO();
            issueDTO.setIssueId(starBeaconDTO.getInstanceId());
            issueDTO.setProjectId(starBeaconDTO.getProjectId());
            if(Objects.isNull(issueMapper.selectOne(issueDTO))) {
                throw new CommonException(ERROR_INSTANCE_IS_NULL);
            }
        } else {
            if (backlogExpandService != null) {
                backlogExpandService.selectBacklogByStar(starBeaconDTO);
            }
        }
        if (starBeaconMapper.insertSelective(starBeaconDTO) != 1){
            throw new CommonException(ERROR_STAR_INSTANCE_FAILED);
        }
    }


    @Override
    public void unStarInstance(StarBeaconVO starBeaconVO) {
        StarBeaconDTO starBeaconDTO = starBeaconAssembler.checkAndSet(starBeaconVO);
        Long userId = DetailsHelper.getUserDetails().getUserId();
        Assert.notNull(userId, ERROR_NOT_LOGIN);
        starBeaconDTO.setUserId(userId);
        starBeaconDTO = starBeaconMapper.selectOne(starBeaconDTO);
        if (Objects.isNull(starBeaconDTO)){
            return;
        }
        if (starBeaconMapper.deleteByPrimaryKey(starBeaconDTO.getId()) != 1) {
            throw new CommonException(ERROR_DELETE_STAR_INSTANCE_FAILED);
        }
    }

}
