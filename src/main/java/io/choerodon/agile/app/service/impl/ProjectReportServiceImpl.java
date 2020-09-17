package io.choerodon.agile.app.service.impl;

import java.io.BufferedOutputStream;
import java.io.IOException;
import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;
import javax.servlet.http.HttpServletResponse;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import io.choerodon.agile.api.vo.ProjectReportVO;
import io.choerodon.agile.api.vo.report.ReportUnitVO;
import io.choerodon.agile.app.service.ProjectReportService;
import io.choerodon.agile.infra.dto.ProjectReportDTO;
import io.choerodon.agile.infra.dto.ProjectReportReceiverDTO;
import io.choerodon.agile.infra.dto.UserDTO;
import io.choerodon.agile.infra.enums.ProjectReportStatus;
import io.choerodon.agile.infra.feign.BaseFeignClient;
import io.choerodon.agile.infra.mapper.ProjectReportMapper;
import io.choerodon.agile.infra.mapper.ProjectReportReceiverMapper;
import io.choerodon.agile.infra.utils.SiteMsgUtil;
import io.choerodon.core.domain.Page;
import io.choerodon.core.exception.CommonException;
import io.choerodon.mybatis.pagehelper.PageHelper;
import io.choerodon.mybatis.pagehelper.domain.PageRequest;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.io.IOUtils;
import org.apache.commons.lang3.ArrayUtils;
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
    private ProjectReportReceiverMapper projectReportReceiverMapper;
    @Autowired
    private BaseFeignClient baseFeignClient;
    @Autowired
    private SiteMsgUtil siteMsgUtil; 
    private ObjectMapper commmonMapper = new ObjectMapper();

    @Override
    public Page<ProjectReportDTO> page(ProjectReportVO projectReport, PageRequest pageRequest) {
        return PageHelper.doPageAndSort(pageRequest, () -> {
            List<ProjectReportVO> page = projectReportMapper.list(projectReport);
            List<Long> reportIdList = page.stream().map(ProjectReportVO::getId).collect(Collectors.toList());
            List<ProjectReportReceiverDTO> receiverDTOList = projectReportReceiverMapper.selectReceiver(reportIdList, 
                    ProjectReportReceiverDTO.TYPE_RECEIVER);
            List<UserDTO> userList = baseFeignClient.listUsersByIds(receiverDTOList.stream()
                    .map(ProjectReportReceiverDTO::getReceiverId).toArray(Long[]::new), false).getBody();
            Map<Long, UserDTO> userDTOMap = userList.stream().collect(Collectors.toMap(UserDTO::getId,
                    Function.identity()));
            Map<Long, List<ProjectReportReceiverDTO>> receiverGroup =
                    receiverDTOList.stream().collect(Collectors.groupingBy(ProjectReportReceiverDTO::getProjectReportId));
            for (ProjectReportVO reportVO : page) {
                reportVO.setReceiverList(receiverGroup
                        .getOrDefault(reportVO.getId(), Collections.emptyList())
                        .stream().map(receiver -> userDTOMap.get(receiver.getReceiverId()))
                        .collect(Collectors.toList()));
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
        // 翻译报表信息单元
        if (StringUtils.isNotBlank(projectReport.getReportData())){
            try {
                projectReportVO.setReportUnitList(commmonMapper.readValue(projectReport.getReportData(),
                        commmonMapper.getTypeFactory().constructParametricType(List.class, ReportUnitVO.class)));
            } catch (IOException e) {
                log.error("json convert failed");
            }
        }
        List<ProjectReportReceiverDTO> receiverList = projectReportReceiverMapper.select(new ProjectReportReceiverDTO(id, projectId));
        if (CollectionUtils.isEmpty(receiverList)){
            return projectReportVO;
        }
        // 设置收件人列表
        Map<String, List<ProjectReportReceiverDTO>> group =
                receiverList.stream().collect(Collectors.groupingBy(ProjectReportReceiverDTO::getType));
        Long[] receiverIds = group.getOrDefault(ProjectReportReceiverDTO.TYPE_RECEIVER, Collections.emptyList())
                .stream().map(ProjectReportReceiverDTO::getReceiverId).toArray(Long[]::new);
        if (ArrayUtils.isNotEmpty(receiverIds)){
            List<UserDTO> userList = baseFeignClient.listUsersByIds(receiverIds, false).getBody();
            projectReportVO.setReceiverList(userList);
        }
        // 设置抄送人列表
        Long[] ccIds = group.getOrDefault(ProjectReportReceiverDTO.TYPE_CC, Collections.emptyList())
                .stream().map(ProjectReportReceiverDTO::getReceiverId).toArray(Long[]::new);
        if (ArrayUtils.isNotEmpty(ccIds)){
            List<UserDTO> userList = baseFeignClient.listUsersByIds(ccIds, false).getBody();
            projectReportVO.setCcList(userList);
        }
        return projectReportVO;
    }

    @Override
    public void create(Long projectId, ProjectReportVO projectReportVO) {
        Assert.notNull(projectReportVO.getTitle(), BaseConstants.ErrorCode.DATA_INVALID);
        Assert.notNull(projectReportVO.getDescription(), BaseConstants.ErrorCode.DATA_INVALID);
        Assert.notNull(projectReportVO.getProjectId(), BaseConstants.ErrorCode.DATA_INVALID);
        Assert.isTrue(CollectionUtils.isNotEmpty(projectReportVO.getReceiverList()), BaseConstants.ErrorCode.DATA_INVALID);
        ProjectReportDTO projectReportDTO = new ProjectReportDTO();
        BeanUtils.copyProperties(projectReportVO, projectReportDTO);
        projectReportDTO.setProjectId(projectId);
        projectReportDTO.setStatus(ProjectReportStatus.ENABLE);
        projectReportDTO.setId(null);
        convertReportUnits(projectReportVO, projectReportDTO);
        projectReportMapper.insertSelective(projectReportDTO);
        createProjectReportReceiver(projectId, projectReportVO, projectReportDTO);
    }

    @Override
    public void delete(Long projectId, Long projectReportId) {
        Assert.notNull(projectId, BaseConstants.ErrorCode.DATA_INVALID);
        Assert.notNull(projectReportId, BaseConstants.ErrorCode.DATA_INVALID);
        List<ProjectReportReceiverDTO> ccDTOList =
                projectReportReceiverMapper.select(new ProjectReportReceiverDTO(projectReportId, projectId));
        if (CollectionUtils.isNotEmpty(ccDTOList)){
            for (ProjectReportReceiverDTO ccDTO : ccDTOList) {
                projectReportReceiverMapper.deleteByPrimaryKey(ccDTO.getId());
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
        Assert.isTrue(CollectionUtils.isNotEmpty(projectReportVO.getReceiverList()), BaseConstants.ErrorCode.DATA_NOT_EXISTS);
        ProjectReportDTO projectReportDTO = new ProjectReportDTO();
        BeanUtils.copyProperties(projectReportVO, projectReportDTO);
        convertReportUnits(projectReportVO, projectReportDTO);
        if (projectReportMapper.updateOptional(projectReportDTO, ProjectReportDTO.FIELD_TITLE, 
                ProjectReportDTO.FIELD_DESCRIPTION, ProjectReportDTO.FIELD_REPORTDATA) != 1){
            throw new CommonException("error.project-report.update.failed");
        }
        // 更新抄送人
        projectReportReceiverMapper.delete(new ProjectReportReceiverDTO(projectReportVO.getId(), projectId));
        createProjectReportReceiver(projectId, projectReportVO, projectReportDTO);
    }

    public void convertReportUnits(ProjectReportVO projectReportVO, ProjectReportDTO projectReportDTO) {
        try {
            projectReportDTO.setReportData(commmonMapper.writeValueAsString(projectReportVO.getReportUnitList()));
        } catch (JsonProcessingException e) {
            log.error("json convert failed");
        }
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
        List<ProjectReportReceiverDTO> ccDTOList = projectReportReceiverMapper.select(new ProjectReportReceiverDTO(id, projectId));
        String imgData = null;
        try {
            imgData = Base64Utils.encodeToString(multipartFile.getBytes());
        } catch (IOException e) {
            e.printStackTrace();
        }
        if (StringUtils.isNotBlank(imgData)){
            siteMsgUtil.sendProjectReport(projectId, ccDTOList.stream().map(ProjectReportReceiverDTO::getReceiverId).collect(Collectors.toList()), imgData);
        }
        

    }

    private void createProjectReportReceiver(Long projectId, ProjectReportVO projectReportVO,
                                            ProjectReportDTO projectReportDTO) {
        Assert.isTrue(CollectionUtils.isNotEmpty(projectReportVO.getReceiverList()), BaseConstants.ErrorCode.DATA_INVALID);
        for (UserDTO userDTO : projectReportVO.getReceiverList()) {
                ProjectReportReceiverDTO projectReportReceiverDTO = new ProjectReportReceiverDTO();
                projectReportReceiverDTO.setProjectId(projectId);
                projectReportReceiverDTO.setProjectReportId(projectReportDTO.getId());
                projectReportReceiverDTO.setType(ProjectReportReceiverDTO.TYPE_RECEIVER);
                projectReportReceiverDTO.setReceiverId(userDTO.getId());
                projectReportReceiverMapper.insertSelective(projectReportReceiverDTO);
            }
        if (CollectionUtils.isNotEmpty(projectReportVO.getCcList())) {
            for (UserDTO userDTO : projectReportVO.getCcList()) {
                ProjectReportReceiverDTO projectReportReceiverDTO = new ProjectReportReceiverDTO();
                projectReportReceiverDTO.setProjectId(projectId);
                projectReportReceiverDTO.setProjectReportId(projectReportDTO.getId());
                projectReportReceiverDTO.setType(ProjectReportReceiverDTO.TYPE_CC);
                projectReportReceiverDTO.setReceiverId(userDTO.getId());
                projectReportReceiverMapper.insertSelective(projectReportReceiverDTO);
            }
        }
    }
}
