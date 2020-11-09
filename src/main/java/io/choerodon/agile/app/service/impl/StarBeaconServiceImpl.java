package io.choerodon.agile.app.service.impl;

import java.util.Objects;

import io.choerodon.agile.api.vo.StarBeaconVO;
import io.choerodon.agile.infra.dto.StarBeaconDTO;
import io.choerodon.agile.infra.mapper.StarBeaconMapper;
import io.choerodon.agile.app.service.StarBeaconService;
import io.choerodon.core.exception.CommonException;
import org.hzero.core.base.BaseConstants;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.util.Assert;

/**
 * 服务实现类
 *
 * @author jiaxu.cui@hand-china.com
 */
@Service
public class StarBeaconServiceImpl implements StarBeaconService {

    @Autowired
    private StarBeaconMapper starBeaconMapper;

    @Override
    public void starIssue(StarBeaconVO starBeaconVO) {
        StarBeaconDTO starBeaconDTO = checkAndSet(starBeaconVO);
        if (Objects.isNull(starBeaconMapper.selectOne(starBeaconDTO))){
            throw new CommonException(BaseConstants.ErrorCode.DATA_EXISTS);
        }
        if (starBeaconMapper.insertSelective(starBeaconDTO) != 1){
            throw new CommonException("error.star_issue.insert.failed");
        }
    }


    @Override
    public void unStarIssue(StarBeaconVO starBeaconVO) {
        StarBeaconDTO starBeaconDTO = checkAndSet(starBeaconVO);
        starBeaconDTO = starBeaconMapper.selectOne(starBeaconDTO);
        if (Objects.isNull(starBeaconDTO)){
            throw new CommonException(BaseConstants.ErrorCode.DATA_INVALID);
        }
        starBeaconMapper.deleteByPrimaryKey(starBeaconDTO.getId());
    }

    private StarBeaconDTO checkAndSet(StarBeaconVO starBeaconVO) {
        Assert.notNull(starBeaconVO.getInstanceId(), BaseConstants.ErrorCode.DATA_INVALID);
        Assert.notNull(starBeaconVO.getOrganizationId(), BaseConstants.ErrorCode.DATA_INVALID);
        Assert.notNull(starBeaconVO.getType(), BaseConstants.ErrorCode.DATA_INVALID);
        Assert.notNull(starBeaconVO.getProjectId(), BaseConstants.ErrorCode.DATA_INVALID);
        Assert.notNull(starBeaconVO.getUserId(), BaseConstants.ErrorCode.DATA_INVALID);
        StarBeaconDTO starBeaconDTO = new StarBeaconDTO();
        starBeaconDTO.setOrganizationId(starBeaconVO.getOrganizationId());
        starBeaconDTO.setProjectId(starBeaconVO.getProjectId());
        starBeaconDTO.setInstanceId(starBeaconVO.getInstanceId());
        starBeaconDTO.setUserId(starBeaconVO.getUserId());
        starBeaconDTO.setType(starBeaconVO.getType());
        return starBeaconDTO;
    }
}
