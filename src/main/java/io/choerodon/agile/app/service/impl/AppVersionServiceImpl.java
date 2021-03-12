package io.choerodon.agile.app.service.impl;

import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.Objects;

import io.choerodon.agile.api.vo.AppVersionVO;
import io.choerodon.agile.app.service.AppVersionService;
import io.choerodon.agile.infra.dto.AppVersionDTO;
import io.choerodon.agile.infra.dto.AppVersionIssueRelDTO;
import io.choerodon.agile.infra.dto.ProductAppVersionRelDTO;
import io.choerodon.agile.infra.dto.SprintDTO;
import io.choerodon.agile.infra.dto.business.SprintConvertDTO;
import io.choerodon.agile.infra.mapper.AppVersionIssueRelMapper;
import io.choerodon.agile.infra.mapper.AppVersionMapper;
import io.choerodon.agile.infra.mapper.ProductAppVersionRelMapper;
import io.choerodon.core.exception.CommonException;

/**
 * @author superlee
 * @since 2021-03-10
 */
@Service
@Transactional(rollbackFor = Exception.class)
public class AppVersionServiceImpl implements AppVersionService {

    @Autowired
    private ModelMapper modelMapper;
    @Autowired
    private AppVersionMapper appVersionMapper;
    @Autowired
    private AppVersionIssueRelMapper appVersionIssueRelMapper;
    @Autowired
    private ProductAppVersionRelMapper productAppVersionRelMapper;

    @Override
    public AppVersionVO createAppVersion(Long projectId, AppVersionVO appVersionVO) {
        if (Boolean.FALSE.equals(checkTagRepeat(appVersionVO))) {
            throw new CommonException("error.appVersion.repeat");
        }
        appVersionVO.setProjectId(projectId);
        AppVersionDTO appVersionDTO = modelMapper.map(appVersionVO, AppVersionDTO.class);
        if (appVersionMapper.insertSelective(appVersionDTO) != 1) {
            throw new CommonException("error.appVersion.insert");
        }
        AppVersionDTO result = appVersionMapper.selectByPrimaryKey(appVersionDTO.getId());
        return modelMapper.map(result, AppVersionVO.class);
    }

    @Override
    public AppVersionVO updateAppVersion(Long projectId, Long appVersionId, AppVersionVO appVersionVO) {
        if (!Objects.equals(projectId, appVersionVO.getProjectId())) {
            throw new CommonException("error.projectId.notEqual");
        }
        if (Boolean.FALSE.equals(checkTagRepeat(appVersionVO))) {
            throw new CommonException("error.appVersion.repeat");
        }

        appVersionVO.setId(appVersionId);
        AppVersionDTO appVersionDTO = modelMapper.map(appVersionVO, AppVersionDTO.class);
        if (appVersionMapper.updateByPrimaryKeySelective(appVersionDTO) != 1) {
            throw new CommonException("error.appVersion.update");
        }
        return modelMapper.map(appVersionMapper.selectByPrimaryKey(appVersionDTO.getId()), AppVersionVO.class);
    }

    @Override
    public AppVersionVO queryAppVersionById(Long projectId, Long appVersionId) {
        AppVersionDTO record = new AppVersionDTO();
        record.setId(appVersionId);
        record.setProjectId(projectId);
        return modelMapper.map(appVersionMapper.selectCount(record), AppVersionVO.class);
    }

    @Override
    public void deleteAppVersion(Long projectId, Long appVersionId) {
        AppVersionDTO record = new AppVersionDTO();
        record.setId(appVersionId);
        record.setProjectId(projectId);

        if (appVersionMapper.selectCount(record) <= 0) {
            throw new CommonException("error.appVersion.notExist");
        }

        AppVersionIssueRelDTO issueRelRecord = new AppVersionIssueRelDTO();
        issueRelRecord.setAppVersionId(appVersionId);
        issueRelRecord.setProjectId(projectId);
        appVersionIssueRelMapper.delete(issueRelRecord);

        ProductAppVersionRelDTO productRelRecord = new ProductAppVersionRelDTO();
        productRelRecord.setAppVersionId(appVersionId);
        productRelRecord.setProjectId(projectId);
        productAppVersionRelMapper.delete(productRelRecord);

        appVersionMapper.deleteByPrimaryKey(appVersionId);
    }

    @Override
    public Boolean checkTagRepeat(AppVersionVO appVersionVO) {
        AppVersionDTO record = new AppVersionDTO();
        record.setGroupId(appVersionVO.getGroupId());
        record.setArtifactId(appVersionVO.getArtifactId());
        record.setVersion(appVersionVO.getVersion());
        return (appVersionMapper.selectCount(record) > 0);
    }
}
