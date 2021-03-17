package io.choerodon.agile.app.service.impl;

import org.apache.commons.lang3.StringUtils;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;
import org.springframework.util.ObjectUtils;
import org.springframework.web.multipart.MultipartFile;
import org.xml.sax.SAXException;

import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;
import javax.xml.parsers.ParserConfigurationException;

import io.choerodon.agile.api.vo.*;
import io.choerodon.agile.app.service.AppVersionService;
import io.choerodon.agile.app.service.PomService;
import io.choerodon.agile.infra.dto.AppVersionDTO;
import io.choerodon.agile.infra.dto.AppVersionIssueRelDTO;
import io.choerodon.agile.infra.dto.ProductAppVersionRelDTO;
import io.choerodon.agile.infra.enums.ProjectCategory;
import io.choerodon.agile.infra.feign.BaseFeignClient;
import io.choerodon.agile.infra.feign.operator.DevopsClientOperator;
import io.choerodon.agile.infra.feign.vo.ProjectCategoryDTO;
import io.choerodon.agile.infra.mapper.AppVersionIssueRelMapper;
import io.choerodon.agile.infra.mapper.AppVersionMapper;
import io.choerodon.agile.infra.mapper.ProductAppVersionRelMapper;
import io.choerodon.agile.infra.utils.ConvertUtil;
import io.choerodon.core.domain.Page;
import io.choerodon.core.exception.CommonException;
import io.choerodon.mybatis.pagehelper.PageHelper;
import io.choerodon.mybatis.pagehelper.domain.PageRequest;

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
    public AppVersionVO createAppVersion(Long projectId, AppVersionCreateVO appVersionCreateVO) {
        AppVersionDTO appVersionDTO = modelMapper.map(appVersionCreateVO, AppVersionDTO.class);
        appVersionDTO.setProjectId(projectId);
        if (Boolean.TRUE.equals(checkRepeat(appVersionDTO))) {
            throw new CommonException("error.appVersion.repeat");
        }
        appVersionDTO.setOrganizationId(ConvertUtil.getOrganizationId(projectId));
        if (appVersionMapper.insertSelective(appVersionDTO) != 1) {
            throw new CommonException("error.appVersion.insert");
        }
        AppVersionDTO result = appVersionMapper.selectByPrimaryKey(appVersionDTO.getId());
        return modelMapper.map(result, AppVersionVO.class);
    }

    @Override
    public AppVersionVO updateAppVersion(Long projectId, Long appVersionId, AppVersionUpdateVO appVersionUpdateVO) {
        AppVersionDTO appVersionDTO = appVersionMapper.selectByPrimaryKey(appVersionId);

        if (ObjectUtils.isEmpty(appVersionDTO)) {
            throw new CommonException("error.appVersion.notExist");
        }
        appVersionDTO.setVersion(appVersionUpdateVO.getVersion());
        appVersionDTO.setVersionAlias(appVersionUpdateVO.getVersionAlias());
        if (Boolean.TRUE.equals(checkRepeat(appVersionDTO))) {
            throw new CommonException("error.appVersion.repeat");
        }
        appVersionDTO.setId(appVersionId);
        appVersionDTO.setObjectVersionNumber(appVersionUpdateVO.getObjectVersionNumber());
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
    public Boolean checkRepeat(Long projectId, AppVersionVO appVersionVO) {
        AppVersionDTO record = new AppVersionDTO();
        record.setProjectId(projectId);
        record.setServiceCode(appVersionVO.getServiceCode());
        record.setArtifactId(appVersionVO.getArtifactId());
        record.setVersion(appVersionVO.getVersion());
        return (appVersionMapper.selectCount(record) > 0);
    }

    private Boolean checkRepeat(AppVersionDTO appVersionDTO) {
        AppVersionDTO record = new AppVersionDTO();
        record.setProjectId(appVersionDTO.getProjectId());
        record.setServiceCode(appVersionDTO.getServiceCode());
        record.setArtifactId(appVersionDTO.getArtifactId());
        record.setVersion(appVersionDTO.getVersion());
        return (appVersionMapper.selectCount(record) > 0);
    }

    @Override
    public List<AppVersionVO> parsePom(Long projectId,
                                       String groupIds,
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
            List<AppVersionVO> appVersionList = pomService.parse(groupIds, pomInputStream, appServiceRepList, organizationId);
            return appVersionList;
        } catch (IOException
                | SAXException
                | ParserConfigurationException e) {
            throw new CommonException("error.parse.pom", e);
        }
    }

    @Override
    public List<AppVersionVO> batchCreateAppVersion(Long projectId, List<AppVersionCreateVO> appVersionCreateVOList) {
        List<AppVersionVO> result = new ArrayList<>();
        if(!CollectionUtils.isEmpty(appVersionCreateVOList)){
            for(AppVersionCreateVO appVersionCreateVO : appVersionCreateVOList) {
                String artifactId =  appVersionCreateVO.getArtifactId();
                String version = appVersionCreateVO.getVersion();
                String serviceCode = appVersionCreateVO.getServiceCode();
                AppVersionDTO dto = new AppVersionDTO();
                dto.setArtifactId(artifactId);
                dto.setVersion(version);
                dto.setServiceCode(serviceCode);
                dto.setProjectId(projectId);
                List<AppVersionDTO> list = appVersionMapper.select(dto);
                if (list.isEmpty()) {
                    result.add(createAppVersion(projectId, appVersionCreateVO));
                } else {
                    dto = list.get(0);
                    AppVersionUpdateVO update = modelMapper.map(dto, AppVersionUpdateVO.class);
                    update.setTag(null);
                    String alias = appVersionCreateVO.getVersionAlias();
                    if (StringUtils.isEmpty(alias)) {
                        update.setVersionAlias(null);
                    } else {
                        update.setVersionAlias(alias);
                    }
                    if (Boolean.FALSE.equals(update.getAppService())
                            && Boolean.TRUE.equals(appVersionCreateVO.getAppService())) {
                        update.setAppService(true);
                    }
                    AppVersionVO vo = updateAppVersion(projectId, dto.getId(), modelMapper.map(dto, AppVersionUpdateVO.class));
                    //设置源appService的值，用于解析父子关系
                    vo.setAppService(appVersionCreateVO.getAppService());
                    result.add(vo);
                }
            }
        }
        return result;
    }

    @Override
    public void deleteAppVersionProductVersionRel(Long projectId, Long appVersionId, Long productVersionId) {
        AppVersionDTO record = new AppVersionDTO();
        record.setId(appVersionId);
        record.setProjectId(projectId);

        if (appVersionMapper.selectCount(record) <= 0) {
            throw new CommonException("error.appVersion.notExist");
        }

        ProductAppVersionRelDTO productRelRecord = new ProductAppVersionRelDTO();
        productRelRecord.setAppVersionId(appVersionId);
        productRelRecord.setProjectId(projectId);
        productRelRecord.setProductVersionId(productVersionId);
        productAppVersionRelMapper.delete(productRelRecord);
    }

    @Override
    public void deleteAppVersionIssueRel(Long projectId, Long appVersionId, Long issueId) {
        AppVersionDTO record = new AppVersionDTO();
        record.setId(appVersionId);
        record.setProjectId(projectId);

        if (appVersionMapper.selectCount(record) <= 0) {
            throw new CommonException("error.appVersion.notExist");
        }

        AppVersionIssueRelDTO issueRelRecord = new AppVersionIssueRelDTO();
        issueRelRecord.setAppVersionId(appVersionId);
        issueRelRecord.setProjectId(projectId);
        issueRelRecord.setIssueId(issueId);
        appVersionIssueRelMapper.delete(issueRelRecord);
    }

    @Override
    public Page<AppVersionVO> listAppVersionByProjectId(Long projectId, AppVersionSearchVO appVersionSearchVO, PageRequest pageRequest) {
        return PageHelper.doPageAndSort(pageRequest, () ->
                appVersionMapper.listAppVersionByProjectId(projectId, appVersionSearchVO));
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
