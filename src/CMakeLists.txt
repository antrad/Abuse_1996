#

# SUBDIRS = lisp net imlib sdlport
add_subdirectory(lisp)
add_subdirectory(net)
add_subdirectory(imlib)
add_subdirectory(sdlport)

if(PLATFORM_WIN32)
    # Add the icon to the abuse.exe file
    set(abuse_RESOURCE_FILES "win32/resources.rc")
else()
    set(abuse_RESOURCE_FILES "")
endif(PLATFORM_WIN32)

add_executable(abuse
    common.h
    lol/matrix.cpp lol/matrix.h
    lol/timer.cpp lol/timer.h
    specache.cpp specache.h
    netcfg.cpp netcfg.h
    innet.cpp
    chat.cpp chat.h
    endgame.cpp
    loadgame.cpp loadgame.h
    profile.cpp profile.h
    cop.cpp cop.h
    statbar.cpp
    compiled.cpp compiled.h
    ant.cpp ant.h
    sensor.cpp
    demo.cpp demo.h
    lcache.cpp lcache.h
    nfclient.cpp nfclient.h
    clisp.cpp clisp.h
    gui.cpp gui.h
    transp.cpp transp.h
    collide.cpp
    property.cpp property.h
    cache.cpp cache.h
    particle.cpp particle.h
    objects.cpp objects.h
    extend.cpp extend.h
    console.cpp console.h
    ability.cpp ability.h
    items.cpp items.h
    dev.cpp dev.h
    chars.cpp chars.h
    level.cpp level.h
    smallfnt.cpp
    automap.cpp automap.h
    help.cpp help.h
    intsect.cpp intsect.h
    loader2.cpp loader2.h
    seq.cpp seq.h
    points.cpp points.h
    fnt6x13.cpp
    morpher.cpp morpher.h
    menu.cpp menu.h
    director.cpp director.h
    view.cpp view.h
    configuration.cpp configuration.h
    game.cpp game.h
    light.cpp light.h
    devsel.cpp devsel.h
    crc.cpp crc.h
    gamma.cpp gamma.h
    id.h netface.h isllist.h sbar.h
    nfserver.h
    ui/volumewindow.cpp ui/volumewindow.h
    ${abuse_RESOURCE_FILES}
)

set_target_properties(abuse PROPERTIES MACOSX_BUNDLE true)
# Note this doesn't install the icon, that's done later
set_target_properties(abuse PROPERTIES MACOSX_BUNDLE_ICON_FILE "abuse")
set_target_properties(abuse PROPERTIES MACOSX_BUNDLE_BUNDLE_NAME "Abuse")
set_target_properties(abuse PROPERTIES MACOSX_BUNDLE_SHORT_VERSION_STRING "${abuse_VERSION}")
set_target_properties(abuse PROPERTIES MACOSX_BUNDLE_BUNDLE_VERSION "${abuse_VERSION}")

target_link_libraries(abuse lisp)
target_link_libraries(abuse sdlport)
target_link_libraries(abuse imlib)
target_link_libraries(abuse net)
target_link_libraries(abuse ${SDL_LIBRARY})
target_link_libraries(abuse ${SDLMIXER_LIBRARY})
if(OPENGL_FOUND)
    target_link_libraries(abuse ${OPENGL_LIBRARIES})
endif(OPENGL_FOUND)

add_executable(abuse-tool tool/abuse-tool.cpp crc.cpp crc.h)

target_link_libraries(abuse-tool imlib)

include_directories(
    ${abuse_SOURCE_DIR}/src
    ${abuse_SOURCE_DIR}/src/lisp
    ${abuse_SOURCE_DIR}/src/imlib
    ${abuse_SOURCE_DIR}/src/net
    ${SDL_INCLUDE_DIR}
    ${SDL_MIXER_INCLUDE_DIR}
)
if(PLATFORM_WINDOWS)
    # Under Windows, throw them into the root
    install(TARGETS abuse abuse-tool
        RUNTIME DESTINATION "."
        LIBRARY DESTINATION ".")
    # We also need to include the SDL DLLs or nothing will work
    string(FIND "${SDL_LIBRARY}" ";" INDEX)
    if (${INDEX} GREATER -1)
        string(SUBSTRING "${SDL_LIBRARY}" 0 ${INDEX} SDL_DLL_PATH)
    else()
        set(SDL_DLL_PATH "${SDL_LIBRARY}")
    endif()
    get_filename_component(SDL_DLL_PATH "${SDL_DLL_PATH}" DIRECTORY)
    install(FILES ${SDL_DLL_PATH}/SDL.dll DESTINATION ".")
    string(FIND "${SDLMIXER_LIBRARY}" ";" INDEX)
    if (${INDEX} GREATER -1)
        string(SUBSTRING "${SDLMIXER_LIBRARY}" 0 ${INDEX} SDLMIXER_DLL_PATH)
    else()
        set(SDLMIXER_DLL_PATH "${SDLMIXER_LIBRARY}")
    endif()
    get_filename_component(SDLMIXER_DLL_PATH "${SDLMIXER_DLL_PATH}" DIRECTORY)
    install(FILES ${SDLMIXER_DLL_PATH}/SDL_mixer.dll DESTINATION ".")
else()
    # Under Linux, we want the tools in bin
    install(TARGETS abuse abuse-tool RUNTIME DESTINATION bin
    BUNDLE DESTINATION "${CMAKE_INSTALL_PREFIX}")
    if(PLATFORM_MAC_OS_X)
        install(FILES "../osx/abuse.icns" DESTINATION "${CMAKE_INSTALL_PREFIX}/abuse.app/Contents/Resources")
    endif()
endif()
