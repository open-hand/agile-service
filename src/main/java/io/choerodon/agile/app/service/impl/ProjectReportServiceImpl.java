package io.choerodon.agile.app.service.impl;

import java.io.BufferedOutputStream;
import java.io.IOException;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.function.Function;
import java.util.stream.Collectors;

import javax.servlet.http.HttpServletResponse;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import io.choerodon.agile.api.vo.ProjectReportVO;
import io.choerodon.agile.api.vo.report.ReportUnitVO;
import io.choerodon.agile.app.service.ProjectReportService;
import io.choerodon.agile.infra.dto.ProjectReportCcDTO;
import io.choerodon.agile.infra.dto.ProjectReportDTO;
import io.choerodon.agile.infra.dto.UserDTO;
import io.choerodon.agile.infra.enums.ProjectReportStatus;
import io.choerodon.agile.infra.feign.BaseFeignClient;
import io.choerodon.agile.infra.mapper.ProjectReportCcMapper;
import io.choerodon.agile.infra.mapper.ProjectReportMapper;
import io.choerodon.agile.infra.utils.SiteMsgUtil;
import io.choerodon.core.domain.Page;
import io.choerodon.core.exception.CommonException;
import io.choerodon.mybatis.pagehelper.PageHelper;
import io.choerodon.mybatis.pagehelper.domain.PageRequest;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.io.IOUtils;
import org.apache.commons.lang3.StringUtils;
import org.hzero.core.base.BaseConstants;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.Assert;
import org.springframework.util.Base64Utils;
import org.springframework.web.multipart.MultipartFile;

/**
 * @author jiaxu.cui@hand-china.com 2020/9/15 上午11:08
 */
@Service
@Transactional(rollbackFor = Exception.class)
public class ProjectReportServiceImpl implements ProjectReportService {
    public static final Logger log = LoggerFactory.getLogger(ProjectReportServiceImpl.class);

    @Autowired
    private ProjectReportMapper projectReportMapper;
    @Autowired
    private ProjectReportCcMapper projectReportCcMapper;
    @Autowired
    private BaseFeignClient baseFeignClient;
    @Autowired
    private SiteMsgUtil siteMsgUtil; 
    private ObjectMapper commmonMapper = new ObjectMapper();

    @Override
    public Page<ProjectReportDTO> page(ProjectReportVO projectReport, PageRequest pageRequest) {
        return PageHelper.doPageAndSort(pageRequest, () -> {
            List<ProjectReportVO> page = projectReportMapper.page(projectReport);
            if (CollectionUtils.isEmpty(page)){
                return page;
            }
            List<Long> ccList = page.stream().map(ProjectReportVO::getReceiverId).collect(Collectors.toList());
            ccList.addAll(page.stream().map(ProjectReportVO::getCreatedBy).collect(Collectors.toList()));
            Map<Long, UserDTO> userDTOMap = baseFeignClient.listUsersByIds(ccList.toArray(new Long[0]), false).getBody()
                    .stream().collect(Collectors.toMap(UserDTO::getId, Function.identity()));
            for (ProjectReportVO projectReportVO : page) {
                projectReportVO.setReceiver(userDTOMap.get(projectReportVO.getReceiverId()));
            }
            return page;
        });
    }

    @Override
    public ProjectReportVO detail(Long projectId, Long id) {
        ProjectReportDTO projectReport = new ProjectReportDTO(id, projectId);
        projectReport = projectReportMapper.selectOne(projectReport);
        if (Objects.isNull(projectReport)){
            return null;
        }
        ProjectReportVO projectReportVO = new ProjectReportVO();
        // 配置基本信息
        BeanUtils.copyProperties(projectReport, projectReportVO);
        projectReportVO.setReceiver(baseFeignClient.listUsersByIds(new Long[]{projectReport.getReceiverId()}, false).getBody().get(0));
        // 翻译报表信息单元
        if (StringUtils.isNotBlank(projectReport.getReportData())){
            try {
                projectReportVO.setReportUnitList(commmonMapper.readValue(projectReport.getReportData(),
                        commmonMapper.getTypeFactory().constructParametricType(List.class, ReportUnitVO.class)));
            } catch (IOException e) {
                log.error("json convert failed");
            }
        }
        // 添加抄送人信息
        List<ProjectReportCcDTO> ccList = projectReportCcMapper.select(new ProjectReportCcDTO(id, projectId));
        if (CollectionUtils.isEmpty(ccList)){
            return projectReportVO;
        }
        Long[] ccIds = ccList.stream().map(ProjectReportCcDTO::getCcId).toArray(Long[]::new);
        List<UserDTO> userList = baseFeignClient.listUsersByIds(ccIds, false).getBody();
        projectReportVO.setCcList(userList);
        return projectReportVO;
    }

    @Override
    public void create(Long projectId, ProjectReportVO projectReportVO) {
        Assert.notNull(projectReportVO.getTitle(), BaseConstants.ErrorCode.DATA_INVALID);
        Assert.notNull(projectReportVO.getDescription(), BaseConstants.ErrorCode.DATA_INVALID);
        Assert.notNull(projectReportVO.getReceiverId(), BaseConstants.ErrorCode.DATA_INVALID);
        Assert.notNull(projectReportVO.getProjectId(), BaseConstants.ErrorCode.DATA_INVALID);
        ProjectReportDTO projectReportDTO = new ProjectReportDTO();
        BeanUtils.copyProperties(projectReportVO, projectReportDTO);
        projectReportDTO.setProjectId(projectId);
        projectReportDTO.setStatus(ProjectReportStatus.ENABLE);
        projectReportDTO.setId(null);
        try {
            projectReportDTO.setReportData(commmonMapper.writeValueAsString(projectReportVO.getReportUnitList()));
        } catch (JsonProcessingException e) {
            log.error("json convert failed");
        }
        projectReportMapper.insertSelective(projectReportDTO);
        createProjectReportCc(projectId, projectReportVO, projectReportDTO);
    }

    @Override
    public void delete(Long projectId, Long projectReportId) {
        Assert.notNull(projectId, BaseConstants.ErrorCode.DATA_INVALID);
        Assert.notNull(projectReportId, BaseConstants.ErrorCode.DATA_INVALID);
        List<ProjectReportCcDTO> ccDTOList =
                projectReportCcMapper.select(new ProjectReportCcDTO(projectReportId, projectId));
        if (CollectionUtils.isNotEmpty(ccDTOList)){
            for (ProjectReportCcDTO ccDTO : ccDTOList) {
                projectReportCcMapper.deleteByPrimaryKey(ccDTO.getId());
            }
        }
        ProjectReportDTO projectReportDTO = new ProjectReportDTO(projectReportId, projectId);
        if (projectReportMapper.delete(projectReportDTO) != 1){
            throw new CommonException("error.project-report.delete.failed");
        }
    }

    @Override
    public void update(Long projectId, ProjectReportVO projectReportVO) {
        Assert.notNull(projectReportVO.getTitle(), BaseConstants.ErrorCode.DATA_INVALID);
        Assert.notNull(projectReportVO.getDescription(), BaseConstants.ErrorCode.DATA_INVALID);
        Assert.notNull(projectReportVO.getReceiverId(), BaseConstants.ErrorCode.DATA_INVALID);
        ProjectReportDTO projectReportDTO = new ProjectReportDTO();
        BeanUtils.copyProperties(projectReportVO, projectReportDTO);
        projectReportMapper.updateOptional(projectReportDTO,
                ProjectReportVO.FIELD_TITLE, ProjectReportVO.FIELD_DESCRIPTION, ProjectReportVO.FIELD_RECEIVERID);
        // 更新抄送人
        projectReportCcMapper.delete(new ProjectReportCcDTO(projectReportVO.getId(), projectId));
        createProjectReportCc(projectId, projectReportVO, projectReportDTO);
    }

    @Override
    public void export(Long projectId, Long id, MultipartFile multipartFile, HttpServletResponse response) {
        try (BufferedOutputStream outputStream = new BufferedOutputStream(response.getOutputStream())){
            response.reset();
            response.setHeader(HttpHeaders.CONTENT_DISPOSITION,
                    String.format("attachment;filename=%s", "项目报告"));
            response.setContentType(MediaType.IMAGE_PNG_VALUE + "charset=" + BaseConstants.DEFAULT_CHARSET);
            response.setHeader(HttpHeaders.CONTENT_LENGTH, "" + multipartFile.getSize());
            response.setHeader(HttpHeaders.CACHE_CONTROL, "must-revalidate, post-check=0, pre-check=0");
            response.setHeader(HttpHeaders.PRAGMA, "public");
            response.setHeader(HttpHeaders.SET_COOKIE, "fileDownload=true; path=/");
            response.setDateHeader(HttpHeaders.EXPIRES, (System.currentTimeMillis() + 1000));
            IOUtils.write(multipartFile.getBytes(), outputStream);
            outputStream.flush();
        } catch (IOException e) {
            throw new CommonException(e);
        }
    }

    @Override
    public void send(Long projectId, Long id, MultipartFile multipartFile) {

        ProjectReportDTO projectReportDTO = projectReportMapper.selectOne(new ProjectReportDTO(id, projectId));
        List<ProjectReportCcDTO> ccDTOList = projectReportCcMapper.select(new ProjectReportCcDTO(id, projectId));
        String imgData = null;
        try {
            imgData = Base64Utils.encodeToString(multipartFile.getBytes());
        } catch (IOException e) {
            e.printStackTrace();
        }
        if (StringUtils.isNotBlank(imgData)){
            siteMsgUtil.sendProjectReport(projectId, projectReportDTO.getReceiverId(), ccDTOList.stream().map(ProjectReportCcDTO::getCcId).collect(Collectors.toList()), imgData);
        }
        

    }

    public void createProjectReportCc(Long projectId, ProjectReportVO projectReportVO,
                                      ProjectReportDTO projectReportDTO) {
        if (Objects.nonNull(projectReportVO.getCcList())) {
            for (UserDTO userDTO : projectReportVO.getCcList()) {
                ProjectReportCcDTO projectReportCcDTO = new ProjectReportCcDTO();
                projectReportCcDTO.setProjectId(projectId);
                projectReportCcDTO.setProjectReportId(projectReportDTO.getId());
                projectReportCcDTO.setCcId(userDTO.getId());
                projectReportCcMapper.insertSelective(projectReportCcDTO);
            }
        }
    }
}
