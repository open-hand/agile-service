package io.choerodon.agile.app.service.impl;

import io.choerodon.agile.api.vo.AppServiceRepVO;
import io.choerodon.agile.api.vo.AppVersionVO;
import io.choerodon.agile.app.service.PomService;
import io.choerodon.core.exception.CommonException;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.ObjectUtils;
import org.w3c.dom.Document;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.List;

/**
 * @author superlee
 * @since 2021-03-12
 */
@Service
@Transactional(rollbackFor = Exception.class)
public class PomServiceImpl implements PomService {

    private static final String DEPENDENCIES = "dependencies";
    private static final String DEPENDENCY = "dependency";
    private static final String GROUP_ID = "groupId";
    private static final String ARTIFACT_ID = "artifactId";
    private static final String VERSION = "version";


    @Override
    public List<AppVersionVO> parse(String inputGroupId,
                                    InputStream inputStream,
                                    List<AppServiceRepVO> appServiceRepList,
                                    Long organizationId)
            throws ParserConfigurationException, IOException, SAXException {
        DocumentBuilder documentBuilder = DocumentBuilderFactory.newInstance().newDocumentBuilder();
        Document document = documentBuilder.parse(inputStream);
        String serviceCode = getArtifactIdFromPom(document);
        Long projectId = fetchProjectId(serviceCode, appServiceRepList);
        if (projectId == null) {
            throw new CommonException("error.illegal.pom.can.not.find.project");
        }
        NodeList nodeList = document.getElementsByTagName(DEPENDENCIES);
        if (nodeList.getLength() == 0) {
            throw new CommonException("error.illegal.pom.missing.dependencies");
        }
        Node dependencies = nodeList.item(0);
        NodeList childNodes = dependencies.getChildNodes();
        return buildAppVersionFromDependency(inputGroupId, organizationId, serviceCode, projectId, childNodes);
    }

    private List<AppVersionVO> buildAppVersionFromDependency(String inputGroupId,
                                                             Long organizationId,
                                                             String serviceCode,
                                                             Long projectId,
                                                             NodeList childNodes) {
        List<AppVersionVO> result = new ArrayList<>();
        for (int i = 0; i < childNodes.getLength(); i++) {
            Node dependency = childNodes.item(i);
            if (DEPENDENCY.equals(dependency.getNodeName())) {
                NodeList dependencyChildNodes = dependency.getChildNodes();
                String groupId = null;
                String artifactId = null;
                String version = null;
                for (int j = 0; j < dependencyChildNodes.getLength(); j++) {
                    Node node = dependencyChildNodes.item(j);
                    if (GROUP_ID.equals(node.getNodeName())) {
                        groupId = node.getTextContent();
                    }
                    if (ARTIFACT_ID.equals(node.getNodeName())) {
                        artifactId = node.getTextContent();
                    }
                    if (VERSION.equals(node.getNodeName())) {
                        version = node.getTextContent();
                    }
                }
                if (inputGroupId.equals(groupId)) {
                    AppVersionVO appVersionVO = new AppVersionVO();
                    appVersionVO.setProjectId(projectId);
                    appVersionVO.setOrganizationId(organizationId);
                    appVersionVO.setServiceCode(serviceCode);
                    appVersionVO.setGroupId(groupId);
                    appVersionVO.setArtifactId(artifactId);
                    appVersionVO.setVersion(version);
                    result.add(appVersionVO);
                }
            }
        }
        return result;
    }

    private String getArtifactIdFromPom(Document document) {
        NodeList pomArtifactIdNodeList = document.getElementsByTagName(ARTIFACT_ID);
        if (pomArtifactIdNodeList.getLength() == 0) {
            throw new CommonException("error.illegal.pom.missing.artifactId");
        }
        return pomArtifactIdNodeList.item(0).getTextContent();
    }

    private Long fetchProjectId(String serviceCode, List<AppServiceRepVO> appServiceRepList) {
        if (ObjectUtils.isEmpty(appServiceRepList)) {
            return null;
        }
        for (AppServiceRepVO vo : appServiceRepList) {
            String code = vo.getCode();
            if (serviceCode.equals(code)) {
                return vo.getProjectId();
            }
        }
        return null;
    }
}
