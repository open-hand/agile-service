package io.choerodon.agile.app.service.impl;

import io.choerodon.agile.api.vo.AppServiceRepVO;
import io.choerodon.agile.api.vo.ProjectVO;
import io.choerodon.agile.app.service.PomService;
import io.choerodon.agile.infra.enums.ProjectCategory;
import io.choerodon.agile.infra.feign.BaseFeignClient;
import io.choerodon.agile.infra.feign.operator.DevopsClientOperator;
import io.choerodon.agile.infra.feign.vo.ProjectCategoryDTO;
import io.choerodon.agile.infra.utils.ConvertUtil;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.io.IOException;
import java.io.InputStream;
import java.util.HashSet;
import java.util.List;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;

import io.choerodon.agile.api.vo.AppVersionVO;
import io.choerodon.agile.app.service.AppVersionService;
import io.choerodon.agile.infra.dto.AppVersionDTO;
import io.choerodon.agile.infra.dto.AppVersionIssueRelDTO;
import io.choerodon.agile.infra.dto.ProductAppVersionRelDTO;
import io.choerodon.agile.infra.mapper.AppVersionIssueRelMapper;
import io.choerodon.agile.infra.mapper.AppVersionMapper;
import io.choerodon.agile.infra.mapper.ProductAppVersionRelMapper;
import io.choerodon.core.exception.CommonException;
import org.springframework.util.ObjectUtils;
import org.springframework.web.multipart.MultipartFile;
import org.xml.sax.SAXException;

import javax.xml.parsers.ParserConfigurationException;

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
    @Autowired
    private BaseFeignClient baseFeignClient;
    @Autowired
    private DevopsClientOperator devopsClientOperator;
    @Autowired
    private PomService pomService;

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

    @Override
    public List<AppVersionVO> parsePom(Long projectId,
                                       String groupId,
                                       MultipartFile multipartFile) {
        ProjectVO project = baseFeignClient.queryProject(projectId).getBody();
        if (project == null) {
            throw new CommonException("error.project.not.existed");
        }
        validateProjectCategories(project);
        List<AppServiceRepVO> appServiceRepList =
                devopsClientOperator.listAppService(projectId, 1, 0, true);
        String fileName = multipartFile.getOriginalFilename();
        if (!fileName.endsWith(".xml")) {
            throw new CommonException("error.illegal.pom.file");
        }
        try {
            Long organizationId = ConvertUtil.getOrganizationId(projectId);
            InputStream pomInputStream = multipartFile.getInputStream();
            List<AppVersionVO> appVersionList = pomService.parse(groupId, pomInputStream, appServiceRepList, organizationId);
            return appVersionList;
        } catch (IOException
                | SAXException
                | ParserConfigurationException e) {
            throw new CommonException("error.parse.pom", e);
        }
    }

    private void validateProjectCategories(ProjectVO project) {
        Set<String> projectCategoryCodes = new HashSet<>();
        if (!ObjectUtils.isEmpty(project.getCategories())) {
            projectCategoryCodes.addAll(
                    project
                            .getCategories()
                            .stream()
                            .map(ProjectCategoryDTO::getCode)
                            .collect(Collectors.toSet()));
        }
        if (!projectCategoryCodes.contains(ProjectCategory.MODULE_DEVOPS)) {
            throw new CommonException("error.project.category.not.contains.devops");
        }
    }
}
