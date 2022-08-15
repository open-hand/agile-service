package io.choerodon.agile.infra.enums;

import java.util.Arrays;
import java.util.List;

/**
 * @author superlee
 * @since 2022-07-11
 */
public class StatusTransferType {

    public static final String SPECIFIER = "specifier";
    public static final String PROJECT_OWNER = "projectOwner";
    public static final String OTHER = "other";
    public static final String ROLE = "role";
    public static final String ASSIGNEE = "assignee";
    public static final String REPORTER = "reporter";
    public static final String PARTICIPANT = "participant";
    public static final String MAIN_RESPONSIBLE = "mainResponsible";
    public static final String RELATED_PARTIES = "relatedParties";

    public static final List<String> ALL_TYPES =
            Arrays.asList(
                    SPECIFIER,
                    PROJECT_OWNER,
                    OTHER,
                    ROLE,
                    ASSIGNEE,
                    REPORTER,
                    PARTICIPANT,
                    MAIN_RESPONSIBLE,
                    RELATED_PARTIES);

    public static final List<String> NOT_DEPEND_ON_ISSUE_DETAILS_TYPES =
            Arrays.asList(
                    SPECIFIER,
                    PROJECT_OWNER,
                    OTHER,
                    ROLE
            );


    public static boolean isSpecifier(String value) {
        return SPECIFIER.equals(value);
    }

    public static boolean isRole(String value) {
        return ROLE.equals(value);
    }

    public static boolean isOther(String value) {
        return OTHER.equals(value);
    }
}
